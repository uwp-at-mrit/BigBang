#include "graphlet/symbol/switchlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "geometry.hpp"
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
	: Switchlet(default_switch_status, radius, thickness, degrees) {
}

Switchlet::Switchlet(SwitchStatus default_status, float radius, float thickness, double degrees)
	: IStatuslet(default_status, &make_default_switch_style)
	, thickness(thickness), size(radius * 2.0F), degrees(degrees) {}

void Switchlet::construct() {
	float epradius = this->thickness;
	float hradius = this->size * 0.5F - epradius;
	
	circle_point(hradius, this->degrees, &this->right_x, &this->right_y);
	circle_point(hradius, this->degrees - 180.0, &this->left_x, &this->left_y);
	circle_point(hradius, this->degrees - 60.00, &this->handle_x, &this->handle_y);

	this->endpoints = geometry_freeze(geometry_union(
		circle(this->left_x, this->left_y, epradius),
		circle(this->right_x, this->right_y, epradius)));
}

void Switchlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, size);
}

double Switchlet::get_direction_degrees() {
	return this->degrees;
}

void Switchlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const SwitchStyle style = this->get_style();
	auto color = (style.color != nullptr) ? style.color : default_color;
	float cx = x + this->size * 0.5F;
	float cy = y + this->size * 0.5F;
	float hx = this->handle_x;
	float hy = this->handle_y;

	if (this->get_value()) {
		hx = this->right_x;
		hy = this->right_y;
	}

	ds->DrawLine(this->left_x + cx, this->left_y + cy, this->right_x + cx, this->right_y + cy, Colours::Background, this->thickness);
	ds->DrawCachedGeometry(this->endpoints, cx, cy, color);
	ds->DrawLine(this->left_x + cx, this->left_y + cy, hx + cx, hy + cy, color, this->thickness);
}
