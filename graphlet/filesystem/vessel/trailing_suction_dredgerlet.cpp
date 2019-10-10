#include "graphlet/filesystem/vessel/trailing_suction_dredgerlet.hpp"

#include "datum/flonum.hpp"
#include "datum/fixnum.hpp"
#include "datum/path.hpp"
#include "datum/file.hpp"

#include "transformation.hpp"
#include "brushes.hxx"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Brushes;

namespace {
	private enum class TSD { TrailingSuctionHopperDredger, GPS, Body, Bridge, Hopper, Suction, Trunnion, Barge };
}

/*************************************************************************************************/
TrailingSuctionDredgerlet::TrailingSuctionDredgerlet(Platform::String^ vessel, Platform::String^ ext, Platform::String^ rootdir) {
	if (vessel != nullptr) {
		this->ms_appdata_config = ms_appdata_file(vessel, ext, rootdir);
	} else {
		this->ms_appdata_config = ref new Uri(ms_apptemp_file("trailing_suction_dredger", ext));
	}
}

TrailingSuctionDredgerlet::~TrailingSuctionDredgerlet() {
}

void TrailingSuctionDredgerlet::construct() {
	this->load(this->ms_appdata_config);
}

void TrailingSuctionDredgerlet::on_appdata(Uri^ vessel, TrailingSuctionDredger^ vessel_config) {
	this->vessel_config = vessel_config;
	this->preview_config = this->vessel_config;

	this->preview_config->fill_boundary(nullptr, nullptr, &this->real_width, &this->real_height);
}

bool TrailingSuctionDredgerlet::ready() {
	return (this->vessel_config != nullptr);
}

void TrailingSuctionDredgerlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOX(w, float(this->real_width));
	SET_BOX(h, float(this->real_height));
}

void TrailingSuctionDredgerlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawRectangle(x, y, Width, Height,
		(this->has_caret() ? Colours::AccentDark : Colours::GrayText),
		2.0F);
}

void TrailingSuctionDredgerlet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	//Platform::String^ hint = file_name_from_path(this->ms_appdata_rootdir);

	//draw_invalid_bitmap(hint, ds, x, y, Width, Height);
}

/*************************************************************************************************/
TrailingSuctionDredger^ TrailingSuctionDredgerlet::clone_vessel(TrailingSuctionDredger^ dest) {
	TrailingSuctionDredger^ clone = ((dest == nullptr) ? ref new TrailingSuctionDredger() : dest);

	clone->refresh(this->vessel_config);

	return clone;
}

void TrailingSuctionDredgerlet::preview(TrailingSuctionDredger^ src) {
	if (this->preview_config == nullptr) {
		this->preview_config = ref new TrailingSuctionDredger(src);
	} else {
		this->preview_config->refresh(src);
	}

	this->preview_config->fill_boundary(nullptr, nullptr, &this->real_width, &this->real_height);
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

	if (src.open(path->Data(), std::ios::in)) {
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

	v_config.open(path->Data(), std::ios::out | std::ios::binary);
	if (v_config.is_open()) {
		write_wtext(v_config, TSD::TrailingSuctionHopperDredger) << "\n\r" << std::endl;

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
	if (src != nullptr) {
		this->refresh(src);
	}
}

void TrailingSuctionDredger::refresh(TrailingSuctionDredger^ src) {
	if (src != nullptr) {
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

void TrailingSuctionDredger::fill_boundary(double* x, double* y, double* width, double* height) {
	size_t ptsize = sizeof(double2);
	double lx = this->ps_suction.x;
	double ty = this->ps_suction.y;
	double rx = this->ps_suction.x;
	double by = this->ps_suction.y;

	region_fuse_point(&lx, &ty, &rx, &by, this->sb_suction.x, this->sb_suction.y);
	region_fuse_point(&lx, &ty, &rx, &by, this->trunnion.x, this->trunnion.y);
	region_fuse_point(&lx, &ty, &rx, &by, this->barge.x, this->barge.y);

	for (size_t idx = 0; idx < sizeof(this->body_vertexes) / ptsize; idx++) {
		region_fuse_point(&lx, &ty, &rx, &by, this->body_vertexes[idx].x, this->body_vertexes[idx].y);
	}

	for (size_t idx = 0; idx < sizeof(this->bridge_vertexes) / ptsize; idx++) {
		region_fuse_point(&lx, &ty, &rx, &by, this->bridge_vertexes[idx].x, this->bridge_vertexes[idx].y);
	}

	SET_VALUES(x, lx, y, ty);
	SET_VALUES(width, rx - lx, height, by - ty);
}
