#include "graphlet/filesystem/configuration/gpslet.hpp"

#include "datum/path.hpp"
#include "datum/file.hpp"

#include "tongue.hpp"

#include "text.hpp"
#include "shape.hpp"
#include "polar.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

namespace {
	private enum class CS { GPS, Ellipsoid, Conversion, Projection };
}

static CanvasSolidColorBrush^ gps_arrow_color = Colours::GhostWhite;
static CanvasSolidColorBrush^ gps_body_color = Colours::Azure;
static CanvasSolidColorBrush^ gps_metrics_color = Colours::Cyan;

static CanvasTextFormat^ gps_N_font = make_bold_text_format("Microsoft Yahei");
static CanvasTextFormat^ gps_metrics_font = make_bold_text_format("Cambria", 14.0F);

static void prepare_gps_style(GPSStyle* style) {
	CAS_SLOT(style->N_font, gps_N_font);
	CAS_SLOT(style->metrics_font, gps_metrics_font);

	CAS_SLOT(style->arrow_color, gps_arrow_color);
	CAS_SLOT(style->arrow_border_color, gps_arrow_color);
	CAS_SLOT(style->color, Colours::Transparent);
	CAS_SLOT(style->border_color, Colours::Transparent);
	CAS_SLOT(style->N_color, style->arrow_color);
	CAS_SLOT(style->N_border_color, style->arrow_border_color);
	CAS_SLOT(style->metrics_color, gps_metrics_color);
	
	FLCAS_SLOT(style->border_thickness, 1.0F);
	FLCAS_SLOT(style->arrow_border_thickness, 2.0F);
	FLCAS_SLOT(style->N_border_thickness, 2.0F);
	ICAS_SLOT(style->metrics_precision, 2U);
}

/*************************************************************************************************/
WarGrey::SCADA::GPSStyle WarGrey::SCADA::default_gps_style(ICanvasBrush^ metrics_color
	, CanvasSolidColorBrush^ arrow_color, double arrow_alpha, CanvasSolidColorBrush^ color, double color_alpha) {
	CanvasSolidColorBrush^ acolor = ((arrow_color == nullptr) ? gps_arrow_color : color);
	GPSStyle style;

	style.arrow_border_color = acolor;
	style.arrow_color = Colours::make(acolor, arrow_alpha);
	style.color = Colours::make((color == nullptr) ? Colours::DarkSlateGray : color, color_alpha);
	style.metrics_color = metrics_color;

	prepare_gps_style(&style);

	return style;
}

/*************************************************************************************************/
GPSlet::GPSlet(Platform::String^ gps, float radius, Platform::String^ ext, Platform::String^ rootdir)
	: GPSlet(default_gps_style(), gps, radius, ext, rootdir) {}

GPSlet::GPSlet(GPSStyle& style, Platform::String^ gps, float radius, Platform::String^ ext, Platform::String^ rootdir)
	: radius(radius), style(style), speed(0.0) {
	if (gps != nullptr) {
		this->ms_appdata_config = ms_appdata_file(gps, ext, rootdir);
	} else {
		// TODO: meanwhile it's useless and easy to be used incorrectly
		this->ms_appdata_config = ref new Uri(ms_apptemp_file("gps", ext));
	}

	prepare_gps_style(&this->style);
	this->arrow_radius = this->radius * 0.5F;
}

GPSlet::~GPSlet() {
	this->unload(this->ms_appdata_config);
}

void GPSlet::construct() {
	this->load(this->ms_appdata_config);
	this->set_north(-90.0);
}

void GPSlet::on_appdata(Uri^ gps, GPSCS^ gps_config) {
	this->gps_config = gps_config;
}

bool GPSlet::ready() {
	return (this->gps_config != nullptr);
}

void GPSlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->radius * 2.0F);
}

void GPSlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->radius;
	float cy = y + this->radius;

	ds->FillCircle(cx, cy, this->radius, this->style.color);
	ds->DrawCircle(cx, cy, this->radius - this->style.border_thickness * 0.5F, this->style.border_color, this->style.border_thickness);

	ds->FillGeometry(this->arrow, cx, cy, this->style.arrow_color);
	ds->DrawGeometry(this->arrow, cx, cy, this->style.arrow_border_color, this->style.arrow_border_thickness);

	ds->FillGeometry(this->N, cx, cy, this->style.N_color);
	ds->DrawGeometry(this->N, cx, cy, this->style.N_border_color, this->style.N_border_thickness);

	ds->FillGeometry(this->knot, cx, cy, this->style.metrics_color);
}

/*************************************************************************************************/
void GPSlet::set_north(double degrees, bool force) {
	if (force || (this->degrees != degrees)) {
		float n_r = this->arrow_radius + (this->radius - this->arrow_radius) * 0.5F;

		this->degrees = degrees;

		this->style.N_font->FontSize = (this->radius - this->arrow_radius) * 0.85F;
		this->arrow = polar_arrowhead(this->arrow_radius, degrees);
		this->N = paragraph("N", degrees, n_r, this->style.N_font);
		this->set_speed(this->speed, true);
	}
}

void GPSlet::set_speed(double knot, bool force) {
	if (force || (this->speed != knot)) {
		this->style.metrics_font->FontSize = this->arrow_radius * 0.5F;
		float m_r = -this->arrow_radius;

		this->speed = knot;
		this->knot = paragraph(flstring(knot, this->style.metrics_precision) + unitspeak("knot"), this->degrees, m_r, this->style.metrics_font);
	}
}

/*************************************************************************************************/
GPSCS^ GPSlet::clone_gpscs(GPSCS^ dest) {
	GPSCS^ clone = ((dest == nullptr) ? ref new GPSCS() : dest);

	clone->refresh(this->gps_config);

	return clone;
}

void GPSlet::refresh(GPSCS^ src) {
	this->store(this->ms_appdata_config, src);
}

/*************************************************************************************************/
GPSCS^ GPSCS::load(Platform::String^ path) {
	GPSCS^ cs = nullptr;
	size_t ptsize = sizeof(double2);
	Platform::String^ wtype;
	std::filebuf src;

	if (open_input_binary(src, path)) {
		cs = ref new GPSCS();
		wtype = read_wtext(src);
		discard_this_line(src);

		if (CS::GPS.ToString()->Equals(wtype)) {
			while (peek_char(src) != EOF) {
				wtype = read_wtext(src, char_end_of_word);

				if (CS::Ellipsoid.ToString()->Equals(wtype)) {
					cs->parameter.a = read_flonum(src);
					cs->parameter.f = read_flonum(src);
					cs->parameter.cm = read_flonum(src);
				} else if (CS::Conversion.ToString()->Equals(wtype)) {
					cs->parameter.cs_tx = read_flonum(src);
					cs->parameter.cs_ty = read_flonum(src);
					cs->parameter.cs_tz = read_flonum(src);
					cs->parameter.cs_s = read_flonum(src);
					cs->parameter.cs_rx = read_flonum(src);
					cs->parameter.cs_ry = read_flonum(src);
					cs->parameter.cs_rz = read_flonum(src);
				} else if (CS::Projection.ToString()->Equals(wtype)) {
					cs->parameter.gk_dx = read_flonum(src);
					cs->parameter.gk_dy = read_flonum(src);
					cs->parameter.gk_dz = read_flonum(src);
					cs->parameter.utm_s = read_flonum(src);
				}

				discard_this_line(src);
			}
		}
	}

	return cs;
}

bool GPSCS::save(GPSCS^ self, Platform::String^ path) {
	std::wofstream v_config;
	bool okay = false;
	size_t ptsize = sizeof(double2);

	if (open_output_binary(v_config, path)) {
		write_wtext(v_config, CS::GPS, true);

		write_wtext(v_config, CS::Ellipsoid);
		v_config << " " << self->parameter.a << " " << self->parameter.f << " " << self->parameter.cm;
		write_newline(v_config);

		write_wtext(v_config, CS::Conversion);
		v_config << " " << self->parameter.cs_tx << " " << self->parameter.cs_ty << " " << self->parameter.cs_tz;
		v_config << " " << self->parameter.cs_s;
		v_config << " " << self->parameter.cs_rx << " " << self->parameter.cs_ry << " " << self->parameter.cs_rz;
		write_newline(v_config);

		write_wtext(v_config, CS::Projection);
		v_config << " " << self->parameter.gk_dx << " " << self->parameter.gk_dy << " " << self->parameter.gk_dz;
		v_config << " " << self->parameter.utm_s;
		write_newline(v_config);

		v_config.flush();

		okay = true;
	}

	return okay;
}

GPSCS::GPSCS(GPSCS^ src) {
	this->refresh(src);
}

void GPSCS::refresh(GPSCS^ src) {
	if ((src != nullptr) && (this != src)) {
		this->parameter = src->parameter;
	}
}
