#include "graphlet/filesystem/project/profilet.hpp"

#include "datum/file.hpp"
#include "datum/fixnum.hpp"

#include "geometry.hpp"
#include "brushes.hxx"
#include "shape.hpp"
#include "paint.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

namespace {
	private enum class TS { Profile, Region, ColorPlot };
}

static Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ default_mark_font = make_bold_text_format("Microsoft Yahei", 10.0F);
static Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ default_slope_style = make_dash_stroke(CanvasDashStyle::Dash);
static Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ default_outline_style = make_roundcap_stroke_style();

static void prepare_profile_style(ProfileStyle* style) {
	CAS_SLOT(style->font, default_mark_font);
	CAS_SLOT(style->ps_draghead_color, Colours::Red);
	CAS_SLOT(style->sb_draghead_color, Colours::Green);

	CAS_SLOT(style->profile_color, Colours::Purple);
	CAS_SLOT(style->profile_style, default_outline_style);
	CAS_SLOT(style->centerline_color, Colours::Crimson);
	CAS_SLOT(style->centerline_style, default_slope_style);
	CAS_SLOT(style->haxes_color, Colours::Tomato);
	CAS_SLOT(style->haxes_style, default_slope_style);
	CAS_SLOT(style->vaxes_color, Colours::DodgerBlue);
	CAS_SLOT(style->vaxes_style, default_slope_style);
	CAS_SLOT(style->border_color, Colours::GrayText);

	FLCAS_SLOT(style->profile_thickness, 1.5F);
	FLCAS_SLOT(style->border_thickness, 1.5F);
	FLCAS_SLOT(style->centerline_thickness, 1.0F);
	FLCAS_SLOT(style->haxes_thickness, 0.5F);
	FLCAS_SLOT(style->vaxes_thickness, 0.5F);

	ICAS_SLOT(style->haxes_count, 4);
	ICAS_SLOT(style->vaxes_half_count, 4);
}

ProfileStyle WarGrey::DTPM::default_profile_style(CanvasSolidColorBrush^ color, CanvasSolidColorBrush^ ps_color, CanvasSolidColorBrush^ sb_color) {
	ProfileStyle style;

	style.profile_color = color;
	style.profile_style = nullptr;
	style.ps_draghead_color = ps_color;
	style.sb_draghead_color = sb_color;

	prepare_profile_style(&style);

	return style;
}

/*************************************************************************************************/
Profilet::Profilet(IVessellet* vessel, Platform::String^ profile, float width, float height, Platform::String^ ext, Platform::String^ rootdir)
	: Profilet(default_profile_style(), vessel, profile, width, height, ext, rootdir) {}

Profilet::Profilet(ProfileStyle& style, IVessellet* vessel, Platform::String^ profile, float width, float height
	, Platform::String^ ext, Platform::String^ rootdir)
	: width(width), height(height), style(style), direction_sign(1.0), profile(nullptr), vessel(vessel) {
	if (profile != nullptr) {
		this->ms_appdata_config = ms_appdata_file(profile, ext, rootdir);
	} else {
		// TODO: meanwhile it's useless and easy to be used incorrectly
		this->ms_appdata_config = ref new Uri(ms_apptemp_file("profile", ext));
	}

	if (this->height == 0.0F) {
		this->height = 200.0F;
	} else if (this->height < 0.0F) {
		this->height *= -this->width;
	}

	prepare_profile_style(&this->style);
}

Profilet::~Profilet() {
	this->unload(this->ms_appdata_config);

	if (this->outline) {
		delete this->outline;
	}
}

void Profilet::construct() {
	this->load(this->ms_appdata_config);
}

void Profilet::update_outline(const Outline* outline, double vessel_x, double vessel_y) {
	if ((!flisnan(vessel_x)) && (!flisnan(vessel_y))) {
		this->vessel_x = vessel_x;
		this->vessel_y = vessel_y;

		if (this->preview_config != nullptr) {
			if (outline != nullptr) {
				if (this->outline == nullptr) {
					this->outline = new Outline(outline);
					this->update_vertical_axes();
				} else if ((this->outline->center_foot.x != outline->center_foot.x) || (this->outline->center_foot.y != outline->center_foot.y)) {
					this->outline->clone_from(outline);
					this->update_vertical_axes();
				}
			} else if (this->outline != nullptr) {
				delete this->outline;
				this->outline = nullptr;
			}

			this->update_outline();
		}
	}
}

void Profilet::on_appdata(Uri^ profile, Profile^ profile_config) {
	this->profile_config = profile_config;

	// avoid updating raw instance accidently
	this->preview_config = ref new Profile(this->profile_config);

	this->update_horizontal_axes();
	this->update_vertical_axes();

	this->update_outline();
}

bool Profilet::ready() {
	return (this->preview_config != nullptr);
}

void Profilet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOX(w, this->width);
	SET_BOX(h, this->height);
}

void Profilet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float border_off = this->style.border_thickness * 0.5F;
	float half_width = this->width * 0.5F;
	float cx = x + half_width;

	if (this->preview_config != nullptr) {
		ds->DrawCachedGeometry(this->haxes, x, y, this->style.haxes_color);
		ds->DrawCachedGeometry(this->vaxes, x, y, this->style.vaxes_color);

		ds->DrawCachedGeometry(this->hmarks, x, y, this->style.haxes_color);
		ds->DrawCachedGeometry(this->vmarks, x, y, this->style.vaxes_color);
	}

	ds->DrawLine(cx, y, cx, y + this->height, this->style.centerline_color, this->style.centerline_thickness, this->style.centerline_style);

	if (this->profile != nullptr) {
		ds->DrawGeometry(this->profile, cx, y, this->style.profile_color, this->style.profile_thickness, this->style.profile_style);
	}

	if ((this->vessel != nullptr) && (this->vessel->ready())) {
		this->vessel->draw_profile(ds, this, cx, y, half_width, this->height);
	}

	ds->DrawRectangle(x + border_off, y + border_off,
		this->width - this->style.border_thickness, this->height - this->style.border_thickness,
		this->style.border_color, this->style.border_thickness);
}

void Profilet::update_horizontal_axes() {
	CanvasGeometry^ marks = blank();
	CanvasPathBuilder^ axes = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float interval = this->height / float(this->style.haxes_count + 1);
	double delta = (this->preview_config->max_depth - this->preview_config->min_depth) / double(this->style.haxes_count + 1);
	float y = this->height - style.haxes_thickness * 0.5F;
	TextExtent mark_te;

	for (int i = 1; i <= this->style.haxes_count; i++) {
		float ythis = y - interval * float(i);
		Platform::String^ mark = flstring(this->preview_config->max_depth - delta * double(i), 1U);
		CanvasGeometry^ gmark = paragraph(mark, this->style.font, &mark_te);

		marks = geometry_union(marks, gmark, style.border_thickness + mark_te.height * 0.618F, ythis - mark_te.height);

		axes->BeginFigure(0.0F, ythis);
		axes->AddLine(this->width, ythis);
		axes->EndFigure(CanvasFigureLoop::Open);
	}

	this->hmarks = geometry_freeze(marks);
	this->haxes = geometry_freeze(geometry_stroke(CanvasGeometry::CreatePath(axes), style.haxes_thickness, style.haxes_style));
}

void Profilet::update_vertical_axes() {
	CanvasPathBuilder^ axes = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	CanvasGeometry^ marks = blank();
	int count = this->style.vaxes_half_count * 2;
	float interval = this->width / float(count + 2);
	float cx = this->width * 0.5F;
	double delta = this->preview_config->width / double(count + 2);
	double start = -this->preview_config->width * 0.5;
	float x = style.haxes_thickness * 0.5F;
	float y = this->height - style.border_thickness;
	TextExtent mark_te;

	for (int i = 0; i <= count + 2; i++) {
		float xthis = x + interval * float(i);
		double distance = (start + delta * double(i)) * this->direction_sign;
		CanvasGeometry^ mark = paragraph(flstring(distance, 0), this->style.font, &mark_te);
		
		if (i != this->style.vaxes_half_count + 1) {
			axes->BeginFigure(xthis, 0.0F);
			axes->AddLine(xthis, this->height);
			axes->EndFigure(CanvasFigureLoop::Open);
		}

		marks = geometry_union(marks, mark, xthis - mark_te.width * 0.5F, y - mark_te.height);
	}

	this->vmarks = geometry_freeze(marks);
	this->vaxes = geometry_freeze(geometry_stroke(CanvasGeometry::CreatePath(axes), style.haxes_thickness, style.haxes_style));
}

void Profilet::update_outline() {
	CanvasPathBuilder^ secpath = nullptr;
	
	if (this->outline != nullptr) {
		double xscale, yscale;
		float2 dotpos;

		this->fill_scale(&xscale, &yscale);

		for (int idx = 0; idx < this->outline->side_count + this->outline->slope_count; idx++) {
			ProfileDot* self = &this->outline->dots[idx];

			if (!flisnan(self->x)) {
				dotpos = this->distance_to_local(self->distance, self->depth, xscale, yscale);

				if (secpath == nullptr) {
					secpath = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
					secpath->BeginFigure(dotpos);
				} else {
					secpath->AddLine(dotpos);
				}
			}
		}
	}

	if (secpath != nullptr) {
		secpath->EndFigure(CanvasFigureLoop::Open);
		this->profile = CanvasGeometry::CreatePath(secpath);
	} else {
		this->profile = nullptr;
	}
}

/*************************************************************************************************/
float2 Profilet::vessel_to_local(double x, double y, double depth) {
	float2 pos(flnan_f, 0.0F);
	double xscale, yscale;

	this->fill_scale(&xscale, &yscale);

	if (this->outline != nullptr) {
		double distance = point_segment_distance(this->vessel_x, this->vessel_y,
			this->outline->center_foot.x, this->outline->center_foot.y, this->outline->center_origin.x, this->outline->center_origin.y);
		
		// NOTE: this is the transverse section which display `y` and `z` and hence `x` is useless
		pos = distance_to_local(distance - y, depth, xscale, yscale);
	} else {
		pos = distance_to_local(y, depth, xscale, yscale);
	}

	return pos;
}

float2 Profilet::distance_to_local(double distance, double depth, double xscale, double yscale) {
	float x = -float(distance * xscale * this->direction_sign);
	float y = +float((depth - ((this->preview_config != nullptr) ? this->preview_config->min_depth : 0.0)) * yscale);

	return float2(x, y);
}

void Profilet::fill_scale(double* xscale, double* yscale) {
	if (this->preview_config != nullptr) {
		double depth_range = (this->preview_config->max_depth - this->preview_config->min_depth);

		SET_BOX(xscale, this->width / this->preview_config->width);
		SET_BOX(yscale, ((depth_range > 0.0) ? this->height / depth_range : 1.0));
	}
}

/*************************************************************************************************/
Profile^ Profilet::clone_profile(Profile^ dest, bool real_profile) {
	Profile^ clone = ((dest == nullptr) ? ref new Profile() : dest);

	clone->refresh(real_profile ? this->profile_config : this->preview_config);

	return clone;
}

void Profilet::preview(Profile^ src) {
	if (src == nullptr) {
		this->preview_config->refresh(this->profile_config);
	} else if (this->preview_config == nullptr) {
		this->preview_config = ref new Profile(src);
	} else {
		this->preview_config->refresh(src);
	}

	this->notify_updated();
}

void Profilet::refresh(Profile^ src) {
	this->store(this->ms_appdata_config, src);
}

/*************************************************************************************************/
Profile^ Profile::load(Platform::String^ path) {
	Profile^ p = nullptr;
	size_t ptsize = sizeof(double2);
	Platform::String^ wtype;
	std::filebuf src;

	if (open_input_binary(src, path)) {
		p = ref new Profile();
		wtype = read_wtext(src);
		discard_this_line(src);

		if (TS::Profile.ToString()->Equals(wtype)) {
			while (peek_char(src) != EOF) {
				wtype = read_wtext(src, char_end_of_word);

				if (TS::Region.ToString()->Equals(wtype)) {
					p->width = read_flonum(src);
					p->min_depth = read_flonum(src);
					p->max_depth = read_flonum(src);
				} else if (TS::ColorPlot.ToString()->Equals(wtype)) {
					p->depth_distance = read_flonum(src);
					p->dragheads_distance = read_flonum(src);
				}

				discard_this_line(src);
			}
		}
	}

	return p;
}

bool Profile::save(Profile^ self, Platform::String^ path) {
	std::wofstream v_config;
	bool okay = false;

	if (open_output_binary(v_config, path)) {
		write_wtext(v_config, TS::Profile, true);

		write_wtext(v_config, TS::Region);
		v_config << " " << self->width << " " << self->min_depth << " " << self->max_depth;
		write_newline(v_config);

		write_wtext(v_config, TS::ColorPlot);
		v_config << " " << self->depth_distance << " " << self->dragheads_distance;
		write_newline(v_config);

		v_config.flush();

		okay = true;
	}

	return okay;
}

Profile::Profile(Profile^ src) {
	this->refresh(src);
}

void Profile::refresh(Profile^ src) {
	if ((src != nullptr) && (this != src)) {
		this->width = src->width;
		this->min_depth = src->min_depth;
		this->max_depth = src->max_depth;
		this->depth_distance = src->depth_distance;
		this->dragheads_distance = src->dragheads_distance;
	}
}
