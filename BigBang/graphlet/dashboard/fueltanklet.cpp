#include "graphlet/dashboard/fueltanklet.hpp"

#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static unsigned int fueltank_default_colors[] = {
	0xF00D0D,
	0xFFB33C,
	0xB4F100, 0xB4F100, 0xB4F100, 0xB4F100, 0xB4F100, 0xB4F100, 0xB4F100, 0xB4F100
};

/*************************************************************************************************/
FuelTanklet::FuelTanklet(float width, float height, ICanvasBrush^ bcolor, GradientStops^ stops)
	: FuelTanklet(0.0F, 1.0F, width, height, bcolor, stops) { }

FuelTanklet::FuelTanklet(float vmin, float vmax, float width, float height, ICanvasBrush^ bcolor, GradientStops^ stops)
	: IRangelet(vmin, vmax), width(width), height(height), thickness(this->width * 0.0618F), border_color(bcolor) {

	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width * 1.618F;
	}

	this->colors = ((stops == nullptr) ? make_gradient_stops(fueltank_default_colors) : stops);
}

void FuelTanklet::construct() {
	float corner_radius = this->thickness * 0.5F;
	float tank_width = this->width * 0.4F / 0.7F;
	float base_height = this->thickness * 1.618F;
	float tank_height = this->height - base_height * 1.8F;
	float monitor_x = this->thickness * 2.0F;
	float monitor_y = this->thickness * 3.6F;
	float monitor_width = tank_width - monitor_x * 2.0F;
	float monitor_height = this->thickness * 1.618F;
	float fitting_x = tank_width - corner_radius;
	float fitting_y = tank_height * 0.45F;
	float fitting_width = this->width - tank_width - this->thickness * 2.0F;
	float fitting_height = this->thickness * 2.0F;

	this->fuel.X = this->thickness;
	this->fuel.Y = this->thickness * 1.618F;
	this->fuel.Width = tank_width - this->fuel.X * 2.0F;
	this->fuel.Height = tank_height - this->fuel.Y * 2.0F;

	auto tank_region = rounded_rectangle(0.0F, 0.0F, tank_width, tank_height, corner_radius, corner_radius);
	auto fuel_region = rectangle(this->fuel);
	
	auto tube = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	CanvasGeometry^ tank_parts[] = {
		geometry_subtract(tank_region, fuel_region),
		rectangle(monitor_x, monitor_y, monitor_width, monitor_height),
		rectangle(0.0F, this->height - base_height, this->width, base_height),
		rounded_rectangle(fitting_x, fitting_y, fitting_width, fitting_height, corner_radius, corner_radius),
	};

	{ // create nozzle
		float radius = this->thickness * 0.5F;
		float tube_start_x = fitting_x + fitting_width - radius;
		float tube_start_y = fitting_y + corner_radius;
		float tube_bottom_y = tank_height - radius * 2.0F - this->thickness * 0.5F;
		float tube_end_x = this->width - radius;
		float tube_end_y = this->height * 0.25F;
		float nozzle_end_x = tube_start_x + radius;
		float nozzle_end_y = this->height * 0.16F;

		tube->BeginFigure(tube_start_x, tube_start_y);
		tube->AddLine(tube_start_x, tube_bottom_y);
		tube->AddArc(float2(tube_end_x, tube_bottom_y), radius, radius, 0.0F, CanvasSweepDirection::CounterClockwise, CanvasArcSize::Small);
		tube->AddLine(tube_end_x, tube_end_y);
		tube->AddLine(nozzle_end_x, nozzle_end_y);
		tube->EndFigure(CanvasFigureLoop::Open);
	}

	this->skeleton = geometry_freeze(geometry_union(geometry_union(tank_parts), // don't mind, it's Visual Studio's fault
		geometry_stroke(CanvasGeometry::CreatePath(tube), this->thickness)));
	this->on_value_changed(0.0F);
}

void FuelTanklet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void FuelTanklet::on_value_changed(float v) {
	this->fuel_color = make_solid_brush(gradient_discrete_color(this->colors, this->get_percentage()));
}

void FuelTanklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float capacity = this->get_percentage();
	
	if (capacity > 0.0F) {
		float fuel_height = fmin(this->fuel.Height * capacity, this->fuel.Height);
		float fuel_x = x + this->fuel.X;
		float fuel_y = y + this->fuel.Y + this->fuel.Height - fuel_height;

		ds->FillRectangle(fuel_x - 1.0F, fuel_y - 1.0F,
			this->fuel.Width + 2.0F, fuel_height + 2.0F,
			this->fuel_color);
	}

	ds->DrawCachedGeometry(this->skeleton, x, y, this->border_color);
}
