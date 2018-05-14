#include "graphlet/symbol/circuit/switchlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static SwitchStatus default_switch_status = SwitchStatus::Normal;
static CanvasSolidColorBrush^ default_color = Colours::GhostWhite;

SwitchStyle WarGrey::SCADA::make_default_switch_style(SwitchStatus status) {
	SwitchStyle s;

	s.color = default_color;

	switch (status) {
	case SwitchStatus::Breakdown: s.color = Colours::Firebrick; break;
	}

	return s;
}

/*************************************************************************************************/
Switchlet::Switchlet(float radius, float thickness, double degrees)
	: Switchlet(default_switch_status, radius, thickness, degrees) {}

Switchlet::Switchlet(SwitchStatus default_status, float radius, float thickness, double degrees)
	: ISymbollet(default_status, &make_default_switch_style, radius, degrees), thickness(thickness) {}

void Switchlet::construct() {
	float epradius = this->thickness * 1.618F;
	float hradius = this->size * 0.5F - epradius;
	
	circle_point(hradius, this->degrees, &this->right_x, &this->right_y);
	circle_point(hradius, this->degrees - 180.0, &this->left_x, &this->left_y);
	circle_point(hradius, this->degrees - 60.00, &this->handle_x, &this->handle_y);

	this->endpoints = geometry_freeze(geometry_union(
		circle(this->left_x, this->left_y, epradius),
		circle(this->right_x, this->right_y, epradius)));
}

void Switchlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const SwitchStyle style = this->get_style();
	auto color = (style.color != nullptr) ? style.color : default_color;
	float cx = x + this->size * 0.5F;
	float cy = y + this->size * 0.5F;
	float sx = this->left_x + cx;
	float sy = this->left_y + cy;
	float hx = this->handle_x;
	float hy = this->handle_y;

	if (this->get_value()) {
		hx = this->right_x;
		hy = this->right_y;
	}

	ds->DrawLine(sx, sy, this->right_x + cx, this->right_y + cy, Colours::Background, this->thickness * 2.0F);
	ds->DrawCachedGeometry(this->endpoints, cx, cy, color);
	ds->DrawLine(sx, sy, hx + cx, hy + cy, color, this->thickness);
}
