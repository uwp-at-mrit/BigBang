#include "graphlet/cylinder/boltlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ bolt_default_color = Colours::Silver;
static CanvasSolidColorBrush^ bolt_default_border_color = Colours::DarkGray;
static CanvasSolidColorBrush^ bolt_default_slide_color = Colours::Gray;
static CanvasSolidColorBrush^ bolt_default_running_color = Colours::ForestGreen;

static float default_thickness = 1.5F;

/*************************************************************************************************/
Boltlet::Boltlet(float radius, double degrees) : Boltlet(BoltState::SlidedOut, radius, degrees) {}

Boltlet::Boltlet(BoltState default_state, float radius, double degrees) : ISymbollet(default_state, radius, degrees) {}

void Boltlet::construct() {
	float thickoff = default_thickness * 0.5F;
	float cylinder_width = this->radiusX * 1.0F;
	float cylinder_height = this->radiusX * 0.618F * 0.618F;
	float cylinder_x = -this->radiusX + thickoff;
	float slide_width = this->width - cylinder_width;
	float slide_height = cylinder_height * 0.618F;
	float slide_in_x = this->radiusX - thickoff - slide_width;
	float slide_out_x = slide_in_x - slide_width * 0.618F;
	float slide_y = -slide_height * 0.5F;
	
	this->slide_in = geometry_rotate(rounded_rectangle(slide_in_x, slide_y, slide_width, slide_height,
		default_thickness, default_thickness), this->degrees, 0.0F, 0.0F);

	this->slide_out = geometry_rotate(rounded_rectangle(slide_out_x, slide_y, slide_width, slide_height,
		default_thickness, default_thickness), this->degrees, 0.0F, 0.0F);

	this->cylinder = geometry_rotate(rounded_rectangle(cylinder_x, -cylinder_height * 0.5F, cylinder_width, cylinder_height,
		default_thickness, default_thickness), this->degrees, 0.0F, 0.0F);
}

void Boltlet::fill_margin(float x, float y, float* top, float* right, float* bottom, float* left) {
	SET_BOX(left, this->enclosing_box.X + this->radiusX);
	SET_BOX(right, this->radiusX - (this->enclosing_box.X + this->enclosing_box.Width));
	SET_BOX(top, this->enclosing_box.Y + this->radiusY);
	SET_BOX(bottom, this->radiusY - (this->enclosing_box.Y + this->enclosing_box.Height));
}

void Boltlet::prepare_style(BoltState status, BoltStyle& style) {
	switch (status) {
	case BoltState::SlidedOut: style.bolt_color = Colours::Yellow; break;
	}

	CAS_SLOT(style.color, bolt_default_color);
	CAS_SLOT(style.border_color, bolt_default_border_color);
	CAS_SLOT(style.bolt_color, bolt_default_slide_color);
	CAS_SLOT(style.running_color, bolt_default_running_color);
}

void Boltlet::on_state_changed(BoltState status) {
	BoltStyle s = this->get_style();
	CanvasGeometry^ slide = nullptr;
	
	switch (status) {
	case BoltState::SlidedIn: slide = this->slide_in; break;
	case BoltState::SlidedOut: slide = this->slide_out; break;
	}

	this->enclosing_box = geometry_union(slide, this->cylinder)->ComputeBounds();
}

void Boltlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	BoltStyle s = this->get_style();
	float cx = x + this->radiusX;
	float cy = y + this->radiusY;

	if (this->get_state() == BoltState::SlidedIn) {
		ds->FillGeometry(this->slide_in, cx, cy, s.bolt_color);
	} else {
		ds->FillGeometry(this->slide_out, cx, cy, s.bolt_color);
	}

	ds->FillGeometry(this->cylinder, cx, cy, (this->running ? s.running_color : s.color));
	ds->DrawGeometry(this->cylinder, cx, cy, s.border_color, default_thickness);
}

void Boltlet::set_running(bool yes_or_no) {
	if (this->running != yes_or_no) {
		this->running = yes_or_no;
		this->notify_updated();
	}
}
