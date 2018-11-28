#include "graphlet/symbol/circuit/switchlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
Switchlet::Switchlet(float radius, float thickness, double degrees)
	: Switchlet(SwitchState::Normal, radius, thickness, degrees) {}

Switchlet::Switchlet(SwitchState default_state, float radius, float thickness, double degrees)
	: ISymbollet(default_state, radius, degrees), thickness(thickness) {}

void Switchlet::construct() {
	float epradius = this->thickness * 1.618F;
	float hradius = this->width * 0.5F - epradius;
	
	circle_point(hradius, this->degrees, &this->right_x, &this->right_y);
	circle_point(hradius, this->degrees - 180.0, &this->left_x, &this->left_y);
	circle_point(hradius, this->degrees - 60.00, &this->handle_x, &this->handle_y);

	this->endpoints = geometry_freeze(geometry_union(
		circle(this->left_x, this->left_y, epradius),
		circle(this->right_x, this->right_y, epradius)));
}

void Switchlet::prepare_style(SwitchState status, SwitchStyle& s) {
	switch (status) {
	case SwitchState::Breakdown: CAS_SLOT(s.color, Colours::Firebrick); break;
	}

	CAS_SLOT(s.color, Colours::GhostWhite);
}

void Switchlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	SwitchStyle style = this->get_style();
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;
	float sx = this->left_x + cx;
	float sy = this->left_y + cy;
	float hx = this->handle_x;
	float hy = this->handle_y;

	if (this->get_value()) {
		hx = this->right_x;
		hy = this->right_y;
	}

	ds->DrawLine(sx, sy, this->right_x + cx, this->right_y + cy, Colours::Background, this->thickness * 2.0F);
	ds->DrawCachedGeometry(this->endpoints, cx, cy, style.color);
	ds->DrawLine(sx, sy, hx + cx, hy + cy, style.color, this->thickness);
}
