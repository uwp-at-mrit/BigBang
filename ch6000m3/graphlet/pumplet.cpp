#include "pumplet.hpp"

#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static PumpState default_pump_state = PumpState::Stopped;
static CanvasSolidColorBrush^ default_body_color = darkgray_brush();
static CanvasSolidColorBrush^ default_border_color = whitesmoke_brush();

PumpStyle WarGrey::SCADA::make_default_pump_style(PumpState state) {
	PumpStyle s;

	s.border_color = default_border_color;
	s.body_color = default_body_color;

	switch (state) {
	case PumpState::Running: s.body_color = dimgray_brush(); s.effect_color = green_brush(); break;
	case PumpState::Starting: s.body_color = dimgray_brush(); s.effect_color = green_brush(); break;
	case PumpState::Unstartable: s.body_color = dimgray_brush(); s.effect_color = green_brush(); break;
	case PumpState::Remote: s.border_color = cyan_brush(); break;
	case PumpState::Stopped: break; // this is the default state
	case PumpState::Stopping: s.effect_color = forest_brush(); break;
	case PumpState::Unstoppable: s.effect_color = forest_brush(); break;
	case PumpState::Ready: s.skeleton_color = cyan_brush(); break;
	}

	return s;
}

Pumplet::Pumplet(float radius, double degree) : Pumplet(default_pump_state, radius, degree) {}

Pumplet::Pumplet(PumpState default_state, float radius, double degree)
	: IStatelet(default_state, &make_default_pump_style), size(radius * 2.0F), degree(degree), thickness(2.0F) {
	this->skeleton = ::triangle(radius - this->thickness * 2.0F, degree);
	this->body = geometry_freeze(this->skeleton);
}

void Pumplet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, size);
}

double Pumplet::get_direction_degree() {
	return this->degree;
}

void Pumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const PumpStyle style = this->get_style();

	float radius = this->size * 0.5F - this->thickness;
	float cx = x + radius + this->thickness;
	float cy = y + radius + this->thickness;
	float bx = cx - radius + this->thickness;
	float by = cy - radius + this->thickness;

	ds->FillCircle(cx, cy, radius, system_background_brush());
	ds->DrawCircle(cx, cy, radius, (style.border_color != nullptr) ? style.border_color : default_border_color, this->thickness);
	ds->DrawCachedGeometry(this->body, bx, by, (style.body_color != nullptr) ? style.body_color : default_body_color);

	if (style.skeleton_color != nullptr) {
		ds->DrawGeometry(this->skeleton, bx, by, style.skeleton_color, this->thickness);
	}

	if (style.effect_color != nullptr) {
		ds->DrawCachedGeometry(this->body, bx, by, style.effect_color);
	}
}
