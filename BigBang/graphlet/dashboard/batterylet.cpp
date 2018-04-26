#include "graphlet/dashboard/batterylet.hpp"

#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

/*************************************************************************************************/
Batterylet::Batterylet(float width, float height, ICanvasBrush^ bcolor, ICanvasBrush^ ncolor, ICanvasBrush^ wcolor, ICanvasBrush^ ecolor)
	: width(width), height(height), thickness(this->width * 0.0618F), border_color(bcolor)
	, normal_color(ncolor), warning_color(wcolor), emergency_color(ecolor) {

	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width * 1.2F;
	}
}

void Batterylet::construct() {
	float battery_y = this->height * 0.1F;
	float base_height = this->thickness * 1.618F;
	float battery_height = this->height - battery_y - base_height * 1.8F;
	float electrode_width = this->thickness * 1.618F;
	float electrode_center_y = this->height * 0.28F;
	float anode_center_x = this->width * 0.75F;
	float anode_x = anode_center_x - electrode_width * 0.5F;
	float anode_sidesize = this->width * 0.15F;
	float anode_y = electrode_center_y - anode_sidesize * 0.5F;
	float anode_hsymbol_x = anode_center_x - anode_sidesize * 0.5F;
	float anode_hsymbol_y = electrode_center_y - this->thickness * 0.5F;
	float anode_vsymbol_x = anode_center_x - this->thickness * 0.5F;
	float anode_vsymbol_y = electrode_center_y - anode_sidesize * 0.5F;
	float cathode_center_x = this->width * 0.25F;
	float cathode_sidesize = this->width * 0.18F;
	float cathode_x = cathode_center_x - electrode_width * 0.5F;
	float cathode_symbol_x = cathode_center_x - cathode_sidesize * 0.5F;
	float cathode_symbol_y = electrode_center_y - this->thickness * 0.5F;
	
	this->electricity.X = this->thickness;
	this->electricity.Y = battery_y + this->thickness;
	this->electricity.Width = this->width - this->electricity.X * 2.0F;
	this->electricity.Height = battery_height - (this->electricity.Y - battery_y) * 2.0F;

	auto battery_region = rectangle(0.0F, battery_y, this->width, battery_height);
	auto electricity_region = rectangle(this->electricity);
	
	CanvasGeometry^ battery_parts[] = {
		geometry_subtract(battery_region, electricity_region),
		rectangle(anode_x, 0.0F, electrode_width, battery_y),
		rectangle(anode_hsymbol_x, anode_hsymbol_y, anode_sidesize, this->thickness),
		rectangle(anode_vsymbol_x, anode_vsymbol_y, this->thickness, anode_sidesize),
		rectangle(cathode_x, 0.0F, electrode_width, battery_y),
		rectangle(cathode_symbol_x, cathode_symbol_y, cathode_sidesize, this->thickness),
		rectangle(0.0F, this->height - base_height, this->width, base_height)
	};

	this->skeleton = geometry_freeze(geometry_union(battery_parts)); // don't mind, it's Visual Studio's fault
}

void Batterylet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Batterylet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float capacity = this->get_scale();

	if (capacity > 0.0F) {
		float fuel_height = fmin(this->electricity.Height * capacity, this->electricity.Height);
		float fuel_x = x + this->electricity.X;
		float fuel_y = y + this->electricity.Y + this->electricity.Height - fuel_height;
		ICanvasBrush^ fuel_color = this->normal_color;

		if (capacity < 0.1F) {
			fuel_color = this->emergency_color;
		} else if (capacity < 0.2F) {
			fuel_color = warning_color;
		}

		ds->FillRectangle(fuel_x - 1.0F, fuel_y - 1.0F, this->electricity.Width + 2.0F, fuel_height + 2.0F, fuel_color);
	}

	ds->DrawCachedGeometry(this->skeleton, x, y, this->border_color);
}
