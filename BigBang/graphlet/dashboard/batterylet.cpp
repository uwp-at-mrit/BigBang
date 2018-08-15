#include "graphlet/dashboard/batterylet.hpp"

#include "shape.hpp"
#include "geometry.hpp"
#include "system.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ battery_default_border_color = WarGrey::SCADA::Colours::make(0xFDFDFD);

static unsigned int battery_default_colors[] = {
	0xF00D0D,
	0xFFB33C,
	0xB4F100, 0xB4F100, 0xB4F100, 0xB4F100, 0xB4F100, 0xB4F100, 0xB4F100, 0xB4F100
};

private class BatteryStatus final : public ISystemStatusListener {
	friend class WarGrey::SCADA::SystemBatterylet;
public:
	void on_battery_capacity_changed(float capacity) {
		this->capacity = capacity;
	}

private:
	float capacity;
};

static BatteryStatus* battery_status = nullptr;

/*************************************************************************************************/
Batterylet::Batterylet(float width, float height, ICanvasBrush^ bcolor, GradientStops^ stops)
	: Batterylet(1.0F, width, height, bcolor, stops) {}

Batterylet::Batterylet(float range, float width, float height, ICanvasBrush^ bcolor, GradientStops^ stops)
	: Batterylet(0.0F, range, width, height, bcolor, stops) {}

Batterylet::Batterylet(float emin, float emax, float width, float height, ICanvasBrush^ bcolor, GradientStops^ stops)
	: IRangelet(emin, emax), width(width), height(height), thickness(3.0F)
	, border_color(bcolor == nullptr ? battery_default_border_color : bcolor) {
	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width * 1.618F;
	}

	this->colors = ((stops == nullptr) ? make_gradient_stops(battery_default_colors) : stops);
}

void Batterylet::construct() {
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
	
	this->charge.X = this->thickness;
	this->charge.Y = battery_y + this->thickness;
	this->charge.Width = this->width - this->charge.X * 2.0F;
	this->charge.Height = battery_height - (this->charge.Y - battery_y) * 2.0F;

	auto battery_region = rounded_rectangle(0.0F, battery_y, this->width, battery_height, corner_radius, corner_radius);
	auto electricity_region = rectangle(this->charge);
	
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

void Batterylet::on_value_changed(float v) {
	this->charge_color = make_solid_brush(gradient_discrete_color(this->colors, this->get_percentage()));
}

void Batterylet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float capacity = this->get_percentage();

	if (capacity > 0.0F) {
		float capacity_height = fmin(this->charge.Height * capacity, this->charge.Height);
		float capacity_x = x + this->charge.X;
		float capacity_y = y + this->charge.Y + this->charge.Height - capacity_height;

		ds->FillRectangle(capacity_x - 1.0F, capacity_y - 1.0F,
			this->charge.Width + 2.0F, capacity_height + 2.0F,
			this->charge_color);
	}

	ds->DrawCachedGeometry(this->skeleton, x, y, this->border_color);
}

/*************************************************************************************************/
SystemBatterylet::SystemBatterylet(float width, float height, ICanvasBrush^ bcolor, GradientStops^ stops)
	: Batterylet(0.0F, 1.0F, width, height, bcolor, stops) {
	if (battery_status == nullptr) {
		battery_status = new BatteryStatus();
		register_system_status_listener(battery_status);
	}
}

void SystemBatterylet::update(long long count, long long interval, long long uptime) {
	this->set_value(battery_status->capacity);
}
