#include "graphlet/device/winchlet.hpp"

#include "polar.hpp"
#include "paint.hpp"
#include "shape.hpp"
#include "brushes.hxx"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/** Warning
 * The dash size will be scaled by thickness
 */
static float winch_motion_dash_style[] = { 4.0F, 2.0F };
static CanvasSolidColorBrush^ winch_default_color = Colours::DarkGray;

/*************************************************************************************************/
Winchlet::Winchlet(float width, float height, float thickness, unsigned int strand)
	: Winchlet(WinchStatus::Stopped, width, height, thickness) {}

Winchlet::Winchlet(WinchStatus default_status, float width, float height, float thickness, unsigned int strand)
	: IStatuslet(default_status), width(width), height(height), strand(strand)
	, thickness(thickness), base_thickness(thickness * 2.0F), cable_thickness(1.0F) {
	if (this->height <= 0.0F) {
		this->height = this->width * 0.618F;
	}
}

void Winchlet::construct() {
	float base_height = this->height - this->base_thickness;
	float base_y = (this->height - base_height) * 0.5F;
	float cable_height = base_height * 0.75F;
	float cable_bunit_width = (this->width - this->base_thickness) / float(this->strand);
	float cable_y = (this->height - cable_height) * 0.5F;
	auto base_style = make_roundcap_stroke_style();
	auto base_l = vline(this->base_thickness * 0.5F, base_y, base_height, this->base_thickness, base_style);
	auto base_r = vline(this->width - this->base_thickness * 0.5F, base_y, base_height, this->base_thickness, base_style);

	{ // construct cable base
		auto cable = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
		unsigned int threshold = this->strand / 2;

		this->cable_style = make_dash_stroke(winch_motion_dash_style);
		this->base = geometry_freeze(geometry_union(base_l, base_r));

		this->cable_base = blank();

		for (unsigned int idx = 0; idx < this->strand; idx++) {
			float cable_base_x = this->base_thickness * 0.25F + cable_bunit_width * float(idx);
			float cable_x = cable_base_x + cable_bunit_width * 0.5F + this->cable_thickness * 0.5F;

			this->cable_base = geometry_union(this->cable_base,
				rounded_rectangle(cable_base_x, cable_y, cable_bunit_width + this->thickness * 0.618F, cable_height,
					this->thickness, this->thickness));

			if (idx >= threshold) {
				cable->BeginFigure(cable_x, cable_y);
				cable->AddLine(cable_x, cable_y + cable_height);
				cable->EndFigure(CanvasFigureLoop::Open);
			}
		}

		this->cable = CanvasGeometry::CreatePath(cable);

		{ // split cable base
			auto cable_base_border = geometry_stroke(this->cable_base, this->thickness);
			
			this->cable_upper = geometry_freeze(geometry_intersect(cable_base_border, this->cable_base, 0.0F, -this->thickness));
			this->cable_bottom = geometry_freeze(geometry_intersect(cable_base_border, this->cable_base, 0.0F, this->thickness));
		}
	}
	
	{ // construct icons
		TextExtent te;
		float radius = cable_height * 0.382F * 0.5F;
		float fdiff = radius * 1.5F * 0.5F;
		auto up_icon = polar_triangle(radius, -90.0);
		auto out_icon = polar_triangle(radius, 90.0);
		auto fup_icon = geometry_union(up_icon, 0.0F, -fdiff, up_icon, 0.0F, fdiff);
		auto fout_icon = geometry_union(out_icon, 0.0F, -fdiff, out_icon, 0.0F, fdiff);
		auto tilde = paragraph("~", make_text_format(16.0F), &te);

		this->icon_cx = this->base_thickness + (this->cable->ComputeBounds().X - this->base_thickness) * 0.5F;
		this->icon_cy = this->height * 0.5F;

		this->icons[_I(WinchStatus::WindingUp)] = up_icon;
		this->icons[_I(WinchStatus::WindingOut)] = out_icon;
		this->icons[_I(WinchStatus::WindUpReady)] = up_icon;
		this->icons[_I(WinchStatus::WindOutReady)] = out_icon;
		this->icons[_I(WinchStatus::Unpullable)] = up_icon;
		this->icons[_I(WinchStatus::Unlettable)] = out_icon;
		this->icons[_I(WinchStatus::FastWindingUp)] = fup_icon;
		this->icons[_I(WinchStatus::FastWindingOut)] = fout_icon;
		this->icons[_I(WinchStatus::FastWindUpReady)] = fup_icon;
		this->icons[_I(WinchStatus::FastWindOutReady)] = fout_icon;
		this->icons[_I(WinchStatus::UpperLimited)] = up_icon;
		this->icons[_I(WinchStatus::LowerLimited)] = out_icon;
		this->icons[_I(WinchStatus::SoftUpperLimited)] = geometry_stroke(up_icon, 1.0F, this->cable_style);
		this->icons[_I(WinchStatus::SoftLowerLimited)] = geometry_stroke(out_icon, 1.0F, this->cable_style);
		this->icons[_I(WinchStatus::SuctionLimited)] = geometry_stroke(circle(radius), 2.0F);
		this->icons[_I(WinchStatus::SpoolLimited)] = polar_rectangle(radius, -60.0, 0.0);
		this->icons[_I(WinchStatus::Slack)] = geometry_translate(tilde, -(te.width + te.lspace) * 0.5F, -te.tspace);
	}
}

void Winchlet::update(long long count, long long interval, long long uptime) {
	if (this->motion_step != 0.0F) {
		switch (this->get_status()) {
		case WinchStatus::WindingUp: case WinchStatus::WindingOut: {
			if (count % 2 == 0) {
				this->cable_style->DashOffset += this->motion_step;
				this->notify_updated();
			}
		}; break;
		case WinchStatus::FastWindingUp: case WinchStatus::FastWindingOut: {
			this->cable_style->DashOffset += this->motion_step;
			this->notify_updated();
		}; break;
		case WinchStatus::Unpullable: case WinchStatus::Unlettable:
		case WinchStatus::UpperLimited: case WinchStatus::LowerLimited:
		case WinchStatus::SoftUpperLimited: case WinchStatus::SoftLowerLimited: {
			if (count % 2 == 1) {
				this->cable_style->DashOffset += this->motion_step;
				this->motion_step = -this->motion_step;
				this->notify_updated();
			}
		}
		}
	}
}

void Winchlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->width);
	SET_BOX(height, this->height);
}

void Winchlet::prepare_style(WinchStatus status, WinchStyle& s) {
	switch (status) {
	case WinchStatus::WindingUp: case WinchStatus::FastWindingUp: {
		CAS_SLOT(s.cable_color, Colours::Green);
	}; break;
	case WinchStatus::WindingOut: case WinchStatus::FastWindingOut: {
		CAS_SLOT(s.cable_color, Colours::Green);
	}; break;
	case WinchStatus::WindUpReady: case WinchStatus::WindOutReady:
	case WinchStatus::FastWindUpReady: case WinchStatus::FastWindOutReady: {
		CAS_SLOT(s.cable_color, Colours::Cyan);
	}; break;
	case WinchStatus::Unpullable: case WinchStatus::Unlettable: {
		CAS_SLOT(s.cable_color, Colours::Red);
	}; break;
	case WinchStatus::UpperLimited: case WinchStatus::SoftUpperLimited: {
		CAS_SLOT(s.cable_color, Colours::YellowGreen);
		CAS_SLOT(s.cable_top_color, Colours::GreenYellow);
	}; break;
	case WinchStatus::LowerLimited: case WinchStatus::SoftLowerLimited: {
		CAS_SLOT(s.cable_color, Colours::YellowGreen);
		CAS_SLOT(s.cable_bottom_color, Colours::GreenYellow);
	}; break;
	case WinchStatus::SuctionLimited: {
		CAS_SLOT(s.status_color, Colours::SeaGreen);
	}; break;
	case WinchStatus::SpoolLimited: case WinchStatus::Slack: {
		CAS_SLOT(s.status_color, Colours::Crimson);
	}; break;
	}

	CAS_SLOT(s.remote_color, Colours::Cyan);
	CAS_SLOT(s.base_color, winch_default_color);
	CAS_SLOT(s.cable_color, winch_default_color);
	CAS_SLOT(s.cable_top_color, s.base_color);
	CAS_SLOT(s.cable_bottom_color, s.base_color);
	CAS_SLOT(s.status_color, s.cable_color);

	// NOTE: The others can be nullptr;
}

void Winchlet::on_status_changed(WinchStatus status) {
	switch (status) {
	case WinchStatus::WindingUp: case WinchStatus::WindUpReady:
	case WinchStatus::FastWindingUp: case WinchStatus::FastWindUpReady: {
		this->motion_step = 1.5F;
	}; break;
	case WinchStatus::WindingOut: case WinchStatus::WindOutReady:
	case WinchStatus::FastWindingOut: case WinchStatus::FastWindOutReady: {
		this->motion_step = -1.5F;
	}; break;
	case WinchStatus::Unpullable: case WinchStatus::UpperLimited: case WinchStatus::SoftUpperLimited: {
		this->motion_step = 1.0F;
	}; break;
	case WinchStatus::Unlettable: case WinchStatus::LowerLimited: case WinchStatus::SoftLowerLimited: {
		this->motion_step = -1.0F;
	}; break;
	default: {
		this->motion_step = 0.0F;
	};
	}
}

void Winchlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	CanvasGeometry^ icon = this->icons[_I(this->get_status())];
	WinchStyle s = this->get_style();

	ds->FillGeometry(this->cable_base, x, y, Colours::Background);
	ds->DrawGeometry(this->cable, x, y, s.cable_color, this->cable_thickness, this->cable_style);
	
	if (icon != nullptr) {
		ds->FillGeometry(icon, x + this->icon_cx, y + this->icon_cy, s.status_color);
	}
	
	ds->DrawCachedGeometry(this->cable_upper, x, y, s.cable_top_color);
	ds->DrawCachedGeometry(this->cable_bottom, x, y, s.cable_bottom_color);

	if (this->remote_control) {
		ds->DrawCachedGeometry(this->base, x, y, s.remote_color);
	} else {
		ds->DrawCachedGeometry(this->base, x, y, s.base_color);
	}
}

void Winchlet::set_remote_control(bool on) {
	this->remote_control = on;
}
