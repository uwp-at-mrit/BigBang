#include "graphlet/dashboard/thermometerlet.hpp"

#include "shape.hpp"
#include "geometry.hpp"
#include "system.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private class BatteryStatus final : public ISystemStatusListener {
	friend class WarGrey::SCADA::Thermometerlet;
public:
	void on_battery_capacity_changed(float capacity) {
		this->capacity = capacity;
	}

private:
	float capacity;
};


static BatteryStatus* battery_status = nullptr;

/*************************************************************************************************/
Thermometerlet::Thermometerlet(float width, float height, ICanvasBrush^ bcolor, ICanvasBrush^ lcolor, ICanvasBrush^ ncolor, ICanvasBrush^ hcolor)
	: width(width), height(height), thickness(this->width * 0.0618F), border_color(bcolor)
	, normal_color(ncolor), low_color(lcolor), high_color(hcolor) {

	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width * 1.618F;
	}
}

void Thermometerlet::construct() {
	float corner_radius = this->thickness * 0.5F;
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
	
	this->temperature.X = this->thickness;
	this->temperature.Y = battery_y + this->thickness;
	this->temperature.Width = this->width - this->temperature.X * 2.0F;
	this->temperature.Height = battery_height - (this->temperature.Y - battery_y) * 2.0F;

	auto meter_region = rounded_rectangle(0.0F, battery_y, this->width, battery_height, corner_radius, corner_radius);
	auto mercury_region = rectangle(this->temperature);
	
	CanvasGeometry^ battery_parts[] = {
		geometry_subtract(meter_region, mercury_region),
		rectangle(anode_x, 0.0F, electrode_width, battery_y),
		rectangle(anode_hsymbol_x, anode_hsymbol_y, anode_sidesize, this->thickness),
		rectangle(anode_vsymbol_x, anode_vsymbol_y, this->thickness, anode_sidesize),
		rectangle(cathode_x, 0.0F, electrode_width, battery_y),
		rectangle(cathode_symbol_x, cathode_symbol_y, cathode_sidesize, this->thickness),
		rectangle(0.0F, this->height - base_height, this->width, base_height)
	};

	this->skeleton = geometry_freeze(geometry_union(battery_parts)); // don't mind, it's Visual Studio's fault

	if (battery_status == nullptr) {
		battery_status = new BatteryStatus();
		register_system_status_listener(battery_status);
	}
}

void Thermometerlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Thermometerlet::update(long long count, long long interval, long long uptime) {
	this->set_value(battery_status->capacity);
}

void Thermometerlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float capacity = this->get_value();

	if (capacity > 0.0F) {
		float capacity_height = fmin(this->temperature.Height * capacity, this->temperature.Height);
		float capacity_x = x + this->temperature.X;
		float capacity_y = y + this->temperature.Y + this->temperature.Height - capacity_height;
		ICanvasBrush^ capacity_color = this->normal_color;

		if (capacity < 0.1F) {
			capacity_color = this->low_color;
		} else if (capacity < 0.2F) {
			capacity_color = high_color;
		}

		ds->FillRectangle(capacity_x - 1.0F, capacity_y - 1.0F,
			this->temperature.Width + 2.0F, capacity_height + 2.0F,
			capacity_color);
	}

	ds->DrawCachedGeometry(this->skeleton, x, y, this->border_color);
}
