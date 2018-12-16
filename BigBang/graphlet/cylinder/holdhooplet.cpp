#include "graphlet/cylinder/holdhooplet.hpp"

#include "math.hpp"
#include "text.hpp"
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

static CanvasSolidColorBrush^ holdhoop_default_color = Colours::Silver;
static CanvasSolidColorBrush^ holdhoop_default_border_color = Colours::DarkGray;
static CanvasSolidColorBrush^ holdhoop_default_hoop_color = Colours::Gray;
static CanvasSolidColorBrush^ holdhoop_default_running_color = Colours::ForestGreen;

static float default_thickness = 1.5F;

/*************************************************************************************************/
HoldHooplet::HoldHooplet(float radius, double degrees) : HoldHooplet(HoldHoopState::Loose, radius, degrees) {}

HoldHooplet::HoldHooplet(HoldHoopState default_state, float radius, double degrees) : ISymbollet(default_state, radius, degrees) {}

void HoldHooplet::construct() {
	float cylinder_radius = this->radiusX * 0.618F;
	float cylinder_height = cylinder_radius * 0.618F;
	float hoop_radius = cylinder_radius * 0.618F;
	float cylinder_distance = this->radiusX - cylinder_radius;
	auto left_hoop = omega(this->degrees - 90.0, hoop_radius, default_thickness * 2.0F);
	auto right_hoop = omega(this->degrees + 90.0, hoop_radius, default_thickness * 2.0F);
	float loose_cx, loose_cy;
	
	circle_point(cylinder_distance, this->degrees + 90.0, &this->cylinder_cx, &this->cylinder_cy);
	circle_point(hoop_radius * 0.382F, this->degrees, &loose_cx, &loose_cy);

	this->held_hoop = geometry_union(left_hoop, right_hoop);
	this->loose_hoop = geometry_union(left_hoop, loose_cx, loose_cy, right_hoop, -loose_cx, -loose_cy);

	this->cylinder = geometry_rotate(rounded_rectangle(-cylinder_radius, -cylinder_height * 0.5F,
		cylinder_radius * 2.0F, cylinder_height, default_thickness, default_thickness),
		this->degrees, 0.0F, 0.0F);
}

void HoldHooplet::fill_margin(float x, float y, float* top, float* right, float* bottom, float* left) {
	SET_BOX(left, this->enclosing_box.X + this->radiusX);
	SET_BOX(right, this->radiusX - (this->enclosing_box.X + this->enclosing_box.Width));
	SET_BOX(top, this->enclosing_box.Y + this->radiusY);
	SET_BOX(bottom, this->radiusY - (this->enclosing_box.Y + this->enclosing_box.Height));
}

void HoldHooplet::prepare_style(HoldHoopState status, HoldHoopStyle& style) {
	switch (status) {
	case HoldHoopState::Loose: style.hoop_color = Colours::Yellow; break;
	}

	CAS_SLOT(style.color, holdhoop_default_color);
	CAS_SLOT(style.border_color, holdhoop_default_border_color);
	CAS_SLOT(style.hoop_color, holdhoop_default_hoop_color);
	CAS_SLOT(style.running_color, holdhoop_default_running_color);
}

void HoldHooplet::on_state_changed(HoldHoopState status) {
	HoldHoopStyle s = this->get_style();
	CanvasGeometry^ hoop = nullptr;
	float hoop_cx, hoop_cy;

	switch (status) {
	case HoldHoopState::Held: hoop = this->held_hoop; break;
	case HoldHoopState::Loose: hoop = this->loose_hoop; break;
	}

	this->fill_hoop_origin(&hoop_cx, &hoop_cy);
	this->enclosing_box = geometry_union(hoop, hoop_cx, hoop_cy,
		this->cylinder, this->cylinder_cx, this->cylinder_cy)->ComputeBounds();
}

void HoldHooplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	HoldHoopStyle s = this->get_style();
	float cx = x + this->radiusX;
	float cy = y + this->radiusY;
	float hoop_cx, hoop_cy;

	this->fill_hoop_origin(&hoop_cx, &hoop_cy);

	if (this->get_state() == HoldHoopState::Held) {
		ds->FillGeometry(this->held_hoop, cx + hoop_cx, cy + hoop_cy, s.hoop_color);
	} else {
		ds->FillGeometry(this->loose_hoop, cx + hoop_cx, cy + hoop_cy, s.hoop_color);
	}

	ds->FillGeometry(this->cylinder, cx + this->cylinder_cx, cy + this->cylinder_cy, (this->running ? s.running_color : s.color));
	ds->DrawGeometry(this->cylinder, cx + this->cylinder_cx, cy + this->cylinder_cy, s.border_color, default_thickness);
}

void HoldHooplet::fill_cylinder_origin(float* cx, float* cy) {
	SET_BOX(cx, this->cylinder_cx);
	SET_BOX(cy, this->cylinder_cy);
}

void HoldHooplet::fill_hoop_origin(float* cx, float* cy) {
	SET_BOX(cx, -this->cylinder_cx);
	SET_BOX(cy, -this->cylinder_cy);
}

void HoldHooplet::set_running(bool yes_or_no) {
	if (this->running != yes_or_no) {
		this->running = yes_or_no;
		this->notify_updated();
	}
}
