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
	: Winchlet(WinchState::Default, width, height, thickness) {}

Winchlet::Winchlet(WinchState default_state, float width, float height, float thickness, unsigned int strand)
	: IStatelet(default_state), width(width), height(height), strand(strand)
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
		float radius = cable_height * 0.382F;
		float sradius = radius * 0.5F;
		float fsdiff = sradius * 1.5F * 0.5F;
		auto sup_icon = polar_triangle(sradius, -90.0);
		auto sout_icon = polar_triangle(sradius, 90.0);
		auto sfup_icon = polar_arrowhead(sradius, -90.0);
		auto sfout_icon = polar_arrowhead(sradius, 90.0);
		auto base_icon = polar_rectangle(sradius, -45.0, 0.0);
		auto up_icon = polar_triangle(radius, -90.0);
		auto out_icon = polar_triangle(radius, 90.0);
		auto uo_icon = geometry_union(sup_icon, 0.0F, -fsdiff, sout_icon, 0.0F, fsdiff);
		auto fup_icon = polar_arrowhead(radius, -90.0);
		auto fout_icon = polar_arrowhead(radius, 90.0);
		auto fuo_icon = geometry_union(sfup_icon, 0.0F, -fsdiff, sfout_icon, 0.0F, fsdiff);
		auto saddle_icon = geometry_union(fout_icon, 0.0F, -fsdiff, base_icon, 0.0F, fsdiff);
		auto tilde = paragraph("~", make_bold_text_format(24.0F), &te);

		this->icon_cx = this->base_thickness + (this->cable->ComputeBounds().X - this->base_thickness) * 0.5F;
		this->icon_cy = this->height * 0.5F;
		
		this->icons[WinchState::WindingUp] = up_icon;
		this->icons[WinchState::WindingOut] = out_icon;
		this->icons[WinchState::WindUpReady] = up_icon;
		this->icons[WinchState::WindOutReady] = out_icon;
		this->icons[WinchState::WindReady] = uo_icon;
		this->icons[WinchState::Unpullable] = up_icon;
		this->icons[WinchState::Unlettable] = out_icon;
		this->icons[WinchState::FastWindingUp] = fup_icon;
		this->icons[WinchState::FastWindingOut] = fout_icon;
		this->icons[WinchState::FastWindUpReady] = fup_icon;
		this->icons[WinchState::FastWindOutReady] = fout_icon;
		this->icons[WinchState::FastWindReady] = fuo_icon;
		this->icons[WinchState::UpperLimited] = up_icon;
		this->icons[WinchState::LowerLimited] = out_icon;
		this->icons[WinchState::SoftUpperLimited] = geometry_stroke(up_icon, 1.0F, this->cable_style);
		this->icons[WinchState::SoftLowerLimited] = geometry_stroke(out_icon, 1.0F, this->cable_style);
		this->icons[WinchState::SuctionLimited] = geometry_stroke(circle(radius), 2.0F);
		this->icons[WinchState::SaddleLimited] = saddle_icon;
		this->icons[WinchState::Slack] = geometry_translate(tilde, -(te.width + te.lspace) * 0.5F, -te.tspace);
		this->icons[WinchState::SuctionSlack] = this->icons[WinchState::SuctionLimited];
		this->icons[WinchState::SaddleSlack] = this->icons[WinchState::SaddleLimited];
	}
}

void Winchlet::update(long long count, long long interval, long long uptime) {
	if (this->motion_step != 0.0F) {
		switch (this->get_state()) {
		case WinchState::WindingUp: case WinchState::WindingOut: {
			if (count % 2 == 0) {
				this->cable_style->DashOffset += this->motion_step;
				this->notify_updated();
			}
		}; break;
		case WinchState::FastWindingUp: case WinchState::FastWindingOut: {
			this->cable_style->DashOffset += this->motion_step;
			this->notify_updated();
		}; break;
		case WinchState::Unpullable: case WinchState::Unlettable:
		case WinchState::UpperLimited: case WinchState::LowerLimited:
		case WinchState::SoftUpperLimited: case WinchState::SoftLowerLimited: {
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

void Winchlet::prepare_style(WinchState status, WinchStyle& s) {
	switch (status) {
	case WinchState::WindingUp: case WinchState::FastWindingUp: {
		CAS_SLOT(s.cable_color, Colours::Green);
	}; break;
	case WinchState::WindingOut: case WinchState::FastWindingOut: {
		CAS_SLOT(s.cable_color, Colours::Green);
	}; break;
	case WinchState::WindUpReady: case WinchState::WindOutReady:
	case WinchState::FastWindUpReady: case WinchState::FastWindOutReady:
	case WinchState::WindReady: case WinchState::FastWindReady: {
		CAS_SLOT(s.cable_color, Colours::Gray);
	}; break;
	case WinchState::Unpullable: case WinchState::Unlettable: {
		CAS_SLOT(s.cable_color, Colours::Red);
	}; break;
	case WinchState::UpperLimited: case WinchState::SoftUpperLimited: {
		CAS_SLOT(s.cable_color, Colours::Yellow);
		CAS_SLOT(s.cable_top_color, Colours::Yellow);
	}; break;
	case WinchState::LowerLimited: case WinchState::SoftLowerLimited: {
		CAS_SLOT(s.cable_color, Colours::Yellow);
		CAS_SLOT(s.cable_bottom_color, Colours::Yellow);
	}; break;
	case WinchState::Slack: {
		CAS_SLOT(s.status_color, Colours::Green);
	}; break;
	case WinchState::SuctionLimited: case WinchState::SuctionSlack: {
		CAS_SLOT(s.status_color, Colours::SeaGreen);
	}; break;
	case WinchState::SaddleLimited: case WinchState::SaddleSlack: {
		CAS_SLOT(s.status_color, Colours::Crimson);
	}; break;
	}

	switch (status) {
	case WinchState::SuctionSlack: case WinchState::SaddleSlack: {
		CAS_SLOT(s.slack_color, Colours::Green);
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

void Winchlet::on_state_changed(WinchState status) {
	switch (status) {
	case WinchState::WindingUp: case WinchState::WindUpReady:
	case WinchState::FastWindingUp: case WinchState::FastWindUpReady: {
		this->motion_step = 1.5F;
	}; break;
	case WinchState::WindingOut: case WinchState::WindOutReady:
	case WinchState::FastWindingOut: case WinchState::FastWindOutReady: {
		this->motion_step = -1.5F;
	}; break;
	case WinchState::Unpullable: case WinchState::UpperLimited: case WinchState::SoftUpperLimited: {
		this->motion_step = 1.0F;
	}; break;
	case WinchState::Unlettable: case WinchState::LowerLimited: case WinchState::SoftLowerLimited: {
		this->motion_step = -1.0F;
	}; break;
	default: {
		this->motion_step = 0.0F;
	};
	}
}

void Winchlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	WinchState status = this->get_state();
	WinchStyle s = this->get_style();
	
	ds->FillGeometry(this->cable_base, x, y, Colours::Background);
	ds->DrawGeometry(this->cable, x, y, s.cable_color, this->cable_thickness, this->cable_style);
	
	if (this->icons.find(status) != this->icons.end()) {
		ds->FillGeometry(this->icons[status], x + this->icon_cx, y + this->icon_cy, s.status_color);

		if (s.slack_color != nullptr) {
			ds->FillGeometry(this->icons[WinchState::Slack], x + this->width - this->icon_cx, y + this->icon_cy, s.slack_color);
		}
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
	if (this->remote_control != on) {
		this->remote_control = on;
		this->notify_updated();
	}
}
