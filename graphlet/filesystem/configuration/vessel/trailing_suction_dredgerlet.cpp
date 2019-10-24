#include "graphlet/filesystem/configuration/vessel/trailing_suction_dredgerlet.hpp"

#include "datum/flonum.hpp"
#include "datum/fixnum.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "brushes.hxx"
#include "math.hpp"
#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

namespace {
	private enum class TSD { TrailingSuctionHopperDredger, GPS, Body, Bridge, Hopper, Suction, Trunnion, Barge };
}

static void prepare_vessel_style(TrailingSuctionDredgerStyle* style) {
	CAS_SLOT(style->body_color, Colours::SkyBlue);
	CAS_SLOT(style->hopper_border, Colours::Khaki);
	CAS_SLOT(style->bridge_border, Colours::RoyalBlue);
	CAS_SLOT(style->gps_color, style->bridge_border);

	CAS_SLOT(style->ps_suction_color, Colours::Red);
	CAS_SLOT(style->sb_suction_color, Colours::Green);
	CAS_SLOT(style->barge_color, Colours::WhiteSmoke);

	FLCAS_SLOT(style->gps_radius, 1.0F);
	FLCAS_SLOT(style->suction_radius, 1.5F);
	FLCAS_SLOT(style->barge_radius, 2.0F);
}

/*************************************************************************************************/
TrailingSuctionDredgerStyle WarGrey::SCADA::default_trailing_suction_dredger_style(ICanvasBrush^ body_color
	, ICanvasBrush^ bridge_border_color, ICanvasBrush^ hopper_border_color) {
	TrailingSuctionDredgerStyle style;

	style.body_color = body_color;
	style.bridge_border = bridge_border_color;
	style.hopper_border = hopper_border_color;

	prepare_vessel_style(&style);

	return style;
}

/*************************************************************************************************/
TrailingSuctionDredgerlet::TrailingSuctionDredgerlet(Platform::String^ vessel, float scale, Platform::String^ ext, Platform::String^ rootdir)
	: TrailingSuctionDredgerlet(vessel, default_trailing_suction_dredger_style(), scale, ext, rootdir) {}

TrailingSuctionDredgerlet::TrailingSuctionDredgerlet(Platform::String^ vessel, TrailingSuctionDredgerStyle& style, float scale, Platform::String^ ext, Platform::String^ rootdir)
	: original_scale(scale), bow_direction(0.0), xscale(1.0F), yscale(1.0F), style(style), ps_drag_exists(false), sb_drag_exists(false) {
	if (vessel != nullptr) {
		this->ms_appdata_config = ms_appdata_file(vessel, ext, rootdir);
	} else {
		// TODO: meanwhile it's useless and easy to be used incorrectly
		this->ms_appdata_config = ref new Uri(ms_apptemp_file("trailing_suction_dredger", ext));
	}

	prepare_vessel_style(&this->style);
}

TrailingSuctionDredgerlet::~TrailingSuctionDredgerlet() {
	this->unload(this->ms_appdata_config);
}

void TrailingSuctionDredgerlet::construct() {
	this->load(this->ms_appdata_config);
}

void TrailingSuctionDredgerlet::on_appdata(Uri^ vessel, TrailingSuctionDredger^ vessel_config) {
	this->vessel_config = vessel_config;

	// avoid updating the raw instance accidently
	this->preview_config = ref new TrailingSuctionDredger(this->vessel_config);
	
	this->clear_boundary();
	this->reconstruct(&this->lt, &this->rb);

	this->xradius = vessel_radius(this->lt, this->rb);
	this->yradius = this->xradius;
}

bool TrailingSuctionDredgerlet::ready() {
	return (this->vessel_config != nullptr);
}

void TrailingSuctionDredgerlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOX(w, this->xradius * this->xscale * 2.0F);
	SET_BOX(h, this->yradius * this->yscale * 2.0F);
}

void TrailingSuctionDredgerlet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	float rx = this->xradius * this->xscale;
	float ry = this->yradius * this->yscale;

	SET_BOX(l, rx + this->lt.x);
	SET_BOX(r, rx - this->rb.x);
	SET_BOX(t, ry + this->lt.y);
	SET_BOX(b, ry - this->rb.y);
}

void TrailingSuctionDredgerlet::reconstruct(float2* lt, float2* rb) {
	size_t ptsize = sizeof(double2);
	size_t bodies = sizeof(this->preview_config->body_vertexes) / ptsize;
	size_t hoppers = sizeof(this->preview_config->hopper_vertexes) / ptsize;
	size_t bridges = sizeof(this->preview_config->bridge_vertexes) / ptsize;
	float2 scale = float2(this->xscale * this->original_scale, this->yscale * this->original_scale);
	double2 gps_pos = this->preview_config->gps[0];

	this->body = vessel_polygon(this->preview_config->body_vertexes, bodies, gps_pos, scale, this->bow_direction, lt, rb);
	this->hopper = vessel_polygon(this->preview_config->hopper_vertexes, hoppers, gps_pos, scale, this->bow_direction, lt, rb);
	this->bridge = vessel_polygon(this->preview_config->bridge_vertexes, bridges, gps_pos, scale, this->bow_direction, lt, rb);

	this->gps[0] = vessel_point(this->preview_config->gps[0], gps_pos, scale, this->bow_direction);
	this->gps[1] = vessel_point(this->preview_config->gps[1], gps_pos, scale, this->bow_direction);
	this->ps_suction = vessel_point(this->preview_config->ps_suction, gps_pos, scale, this->bow_direction);
	this->sb_suction = vessel_point(this->preview_config->sb_suction, gps_pos, scale, this->bow_direction);
	this->trunnion = vessel_point(this->preview_config->trunnion, gps_pos, scale, this->bow_direction);
	this->barge = vessel_point(this->preview_config->barge, gps_pos, scale, this->bow_direction);
}

void TrailingSuctionDredgerlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->xradius * this->xscale;
	float cy = y + this->yradius * this->yscale;

	if (this->body != nullptr) {
		// NOTE that the map uses lefthand coordinate system, the xscale and yscale therefore should be interchanged
		// Stupid design, and/or stupid referenced codebase for lacking of explanation
		float rsx = this->yscale * this->original_scale;
		float rsy = this->xscale * this->original_scale;
		
		draw_geometry(ds, this->body, cx, cy, this->style.body_color, this->style.body_border);
		draw_geometry(ds, this->hopper, cx, cy, this->style.hopper_color, this->style.hopper_border);
		draw_geometry(ds, this->bridge, cx, cy, this->style.bridge_color, this->style.bridge_border);

		for (unsigned int idx = 0; idx < sizeof(this->gps) / sizeof(float2); idx++) {
			if (this->bridge->FillContainsPoint(this->gps[idx]) || this->body->FillContainsPoint(this->gps[idx])) {
				ds->DrawEllipse(this->gps[idx].x + cx, this->gps[idx].y + cy,
					this->style.gps_radius * rsx, this->style.gps_radius * rsy,
					this->style.gps_color);
			}
		}

		if (this->body->FillContainsPoint(this->ps_suction)) {
			ds->DrawEllipse(this->ps_suction.x + cx, this->ps_suction.y + cy,
				this->style.suction_radius * rsx, this->style.suction_radius * rsy,
				this->style.ps_suction_color);
		}

		if (this->body->FillContainsPoint(this->sb_suction)) {
			ds->DrawEllipse(this->sb_suction.x + cx, this->sb_suction.y + cy,
				this->style.suction_radius * rsx, this->style.suction_radius * rsy,
				this->style.sb_suction_color);
		}

		if (this->body->FillContainsPoint(this->barge)) {
			ds->DrawEllipse(this->barge.x + cx, this->barge.y + cy,
				this->style.barge_radius * rsx, this->style.barge_radius * rsy,
				this->style.barge_color);
		}
	}
}

void TrailingSuctionDredgerlet::resize(float width, float height) {
	bool resized = false;

	if ((width > 0.0F) && (height > 0.0F)) {
		float sx = width * 0.5F / this->xradius;
		float sy = height * 0.5F / this->yradius;

		if (this->xscale != sx) {
			this->xscale = sx;
			resized |= true;
		}

		if (this->yscale != sy) {
			this->yscale = sy;
			resized |= true;
		}
	}

	if (resized) {
		this->reconstruct();
		this->notify_updated();
	}
}

void TrailingSuctionDredgerlet::set_ps_drag_figures(double3& offset, double3 ujoints[], size_t joint_count, double3& draghead) {
	this->ps_drag_exists = true;

	this->get_logger()->log_message(Log::Info, L"PS: Offset: (%f, %f)", offset.x, offset.y);
	this->get_logger()->log_message(Log::Info, L"PS: Joint1: (%f, %f)", ujoints[0].x, ujoints[0].y);
	this->get_logger()->log_message(Log::Info, L"PS: Joint2: (%f, %f)", ujoints[1].x, ujoints[1].y);
	this->get_logger()->log_message(Log::Info, L"PS: Draghead: (%f, %f)", draghead.x, draghead.y);
}

void TrailingSuctionDredgerlet::set_sb_drag_figures(double3& offset, double3 ujoints[], size_t joint_count, double3& draghead) {
	this->sb_drag_exists = true;

	this->get_logger()->log_message(Log::Info, L"SB: Offset: (%f, %f)", offset.x, offset.y);
	this->get_logger()->log_message(Log::Info, L"SB: Joint1(%f, %f)", ujoints[0].x, ujoints[0].y);
	this->get_logger()->log_message(Log::Info, L"SB: Joint2(%f, %f)", ujoints[1].x, ujoints[1].y);
	this->get_logger()->log_message(Log::Info, L"SB: Draghead: (%f, %f)", draghead.x, draghead.y);
}

/*************************************************************************************************/
void TrailingSuctionDredgerlet::set_bow_direction(double degrees) {
	if (this->ready()) {
		if (this->bow_direction != degrees) {
			this->bow_direction = degrees;

			this->clear_boundary();
			this->reconstruct(&this->lt, &this->rb);

			this->notify_updated();
		}
	}
}

Size TrailingSuctionDredgerlet::original_size() {
	return Size(this->xradius * 2.0F, this->yradius * 2.0F);
}

/*************************************************************************************************/
TrailingSuctionDredger^ TrailingSuctionDredgerlet::clone_vessel(TrailingSuctionDredger^ dest, bool real_vessel) {
	TrailingSuctionDredger^ clone = ((dest == nullptr) ? ref new TrailingSuctionDredger() : dest);

	clone->refresh(real_vessel ? this->vessel_config : this->preview_config);

	return clone;
}

void TrailingSuctionDredgerlet::preview(TrailingSuctionDredger^ src) {
	if (src == nullptr) {
		this->preview_config->refresh(this->vessel_config);
	} else if (this->preview_config == nullptr) {
		this->preview_config = ref new TrailingSuctionDredger(src);
	} else {
		this->preview_config->refresh(src);
	}

	this->reconstruct();
}

void TrailingSuctionDredgerlet::refresh(TrailingSuctionDredger^ src) {
	this->store(this->ms_appdata_config, src);
}

/*************************************************************************************************/
TrailingSuctionDredger^ TrailingSuctionDredger::load(Platform::String^ path) {
	TrailingSuctionDredger^ dredger = nullptr;
	size_t ptsize = sizeof(double2);
	Platform::String^ wtype;
	std::filebuf src;

	if (open_input_binary(src, path)) {
		dredger = ref new TrailingSuctionDredger();
		wtype = read_wtext(src);
		discard_this_line(src);

		if (TSD::TrailingSuctionHopperDredger.ToString()->Equals(wtype)) {
			while (peek_char(src) != EOF) {
				wtype = read_wtext(src, char_end_of_word);

				if (TSD::GPS.ToString()->Equals(wtype)) {
					unsigned long long n = read_natural(src);
					size_t size = sizeof(dredger->gps) / ptsize;

					for (unsigned long long idx = 0; idx < fxmin(n, size); idx++) {
						dredger->gps[idx].x = read_flonum(src);
						dredger->gps[idx].y = read_flonum(src);
					}
				} else if (TSD::Body.ToString()->Equals(wtype)) {
					unsigned long long n = read_natural(src);
					size_t size = sizeof(dredger->body_vertexes) / ptsize;

					for (unsigned long long idx = 0; idx < fxmin(n, size); idx++) {
						dredger->body_vertexes[idx].x = read_flonum(src);
						dredger->body_vertexes[idx].y = read_flonum(src);
					}
				} else if (TSD::Hopper.ToString()->Equals(wtype)) {
					unsigned long long n = read_natural(src);
					size_t size = sizeof(dredger->hopper_vertexes) / ptsize;

					for (unsigned long long idx = 0; idx < fxmin(n, size); idx++) {
						dredger->hopper_vertexes[idx].x = read_flonum(src);
						dredger->hopper_vertexes[idx].y = read_flonum(src);
					}
				} else if (TSD::Bridge.ToString()->Equals(wtype)) {
					unsigned long long n = read_natural(src);
					size_t size = sizeof(dredger->bridge_vertexes) / ptsize;

					for (unsigned long long idx = 0; idx < fxmin(n, size); idx++) {
						dredger->bridge_vertexes[idx].x = read_flonum(src);
						dredger->bridge_vertexes[idx].y = read_flonum(src);
					}
				} else if (TSD::Suction.ToString()->Equals(wtype)) {
					dredger->ps_suction.x = read_flonum(src);
					dredger->ps_suction.y = read_flonum(src);
					dredger->sb_suction.x = read_flonum(src);
					dredger->sb_suction.y = read_flonum(src);
				} else if (TSD::Trunnion.ToString()->Equals(wtype)) {
					dredger->trunnion.x = read_flonum(src);
					dredger->trunnion.y = read_flonum(src);
				} else if (TSD::Barge.ToString()->Equals(wtype)) {
					dredger->barge.x = read_flonum(src);
					dredger->barge.y = read_flonum(src);
				}

				discard_this_line(src);
			}
		}
	}

	return dredger;
}

bool TrailingSuctionDredger::save(TrailingSuctionDredger^ self, Platform::String^ path) {
	std::wofstream v_config;
	bool okay = false;
	size_t ptsize = sizeof(double2);

	if (open_output_binary(v_config, path)) {
		write_wtext(v_config, TSD::TrailingSuctionHopperDredger, true);

		write_wtext(v_config, TSD::Body) << " " << sizeof(self->body_vertexes) / ptsize;
		for (size_t idx = 0; idx < sizeof(self->body_vertexes) / ptsize; idx++) {
			write_position(v_config, self->body_vertexes[idx]);
		}
		write_newline(v_config);

		write_wtext(v_config, TSD::Bridge) << " " << sizeof(self->bridge_vertexes) / ptsize;
		for (size_t idx = 0; idx < sizeof(self->bridge_vertexes) / ptsize; idx++) {
			write_position(v_config, self->bridge_vertexes[idx]);
		}
		write_newline(v_config);

		write_wtext(v_config, TSD::Hopper) << " " << sizeof(self->hopper_vertexes) / ptsize;
		for (size_t idx = 0; idx < sizeof(self->hopper_vertexes) / ptsize; idx++) {
			write_position(v_config, self->hopper_vertexes[idx]);
		}
		write_newline(v_config);

		write_wtext(v_config, TSD::GPS) << " " << sizeof(self->gps) / ptsize;
		for (size_t idx = 0; idx < sizeof(self->gps) / ptsize; idx++) {
			write_position(v_config, self->gps[idx]);
		}
		write_newline(v_config);

		write_wtext(v_config, TSD::Suction);
		write_position(v_config, self->ps_suction);
		write_position(v_config, self->sb_suction);
		write_newline(v_config);

		write_wtext(v_config, TSD::Trunnion);
		write_position(v_config, self->trunnion);
		write_newline(v_config);

		write_wtext(v_config, TSD::Barge);
		write_position(v_config, self->barge);
		write_newline(v_config);

		v_config.flush();

		okay = true;
	}

	return okay;
}

TrailingSuctionDredger::TrailingSuctionDredger(TrailingSuctionDredger^ src) {
	this->refresh(src);
}

void TrailingSuctionDredger::refresh(TrailingSuctionDredger^ src) {
	if ((src != nullptr) && (this != src)) {
		size_t ptsize = sizeof(double2);

		this->ps_suction = src->ps_suction;
		this->sb_suction = src->sb_suction;
		this->trunnion = src->trunnion;
		this->barge = src->barge;

		for (size_t idx = 0; idx < sizeof(this->gps) / ptsize; idx++) {
			this->gps[idx] = src->gps[idx];
		}

		for (size_t idx = 0; idx < sizeof(this->body_vertexes) / ptsize; idx++) {
			this->body_vertexes[idx] = src->body_vertexes[idx];
		}

		for (size_t idx = 0; idx < sizeof(this->hopper_vertexes) / ptsize; idx++) {
			this->hopper_vertexes[idx] = src->hopper_vertexes[idx];
		}

		for (size_t idx = 0; idx < sizeof(this->bridge_vertexes) / ptsize; idx++) {
			this->bridge_vertexes[idx] = src->bridge_vertexes[idx];
		}
	}
}
