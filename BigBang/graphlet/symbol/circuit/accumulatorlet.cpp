#include "graphlet/symbol/circuit/accumulatorlet.hpp"

#include "shape.hpp"
#include "geometry.hpp"
#include "system.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static AccumulatorStatus default_accumulator_status = AccumulatorStatus::Normal;
static CanvasSolidColorBrush^ default_color = Colours::GhostWhite;
static CanvasSolidColorBrush^ default_background_color = Colours::DimGray;

AccumulatorStyle WarGrey::SCADA::make_default_accumulator_style(AccumulatorStatus status) {
	AccumulatorStyle s;

	s.color = default_color;
	s.bgcolor = default_background_color;

	switch (status) {
	case AccumulatorStatus::Breakdown: s.color = Colours::Firebrick; break;
	}

	return s;
}

/*************************************************************************************************/
Accumulatorlet::Accumulatorlet(float radius, float thickness, double degrees)
	: Accumulatorlet(default_accumulator_status, radius, thickness, degrees) {}

Accumulatorlet::Accumulatorlet(AccumulatorStatus default_status, float radius, float thickness, double degrees)
	: ISymbollet(default_status, &make_default_accumulator_style, radius, degrees), thickness(thickness) {}

void Accumulatorlet::construct() {
	float corner_radius = this->thickness * 0.5F;
	float battery_y = this->size * 0.1F;
	float battery_height = this->size - battery_y - this->thickness;
	float electrode_width = this->thickness * 1.618F;
	float electrode_center_y = this->size * 0.28F;
	float anode_center_x = this->size * 0.75F;
	float anode_x = anode_center_x - electrode_width * 0.5F;
	float anode_sidesize = this->size * 0.15F;
	float anode_y = electrode_center_y - anode_sidesize * 0.5F;
	float anode_hsymbol_x = anode_center_x - anode_sidesize * 0.5F;
	float anode_hsymbol_y = electrode_center_y - this->thickness * 0.5F;
	float anode_vsymbol_x = anode_center_x - this->thickness * 0.5F;
	float anode_vsymbol_y = electrode_center_y - anode_sidesize * 0.5F;
	float cathode_center_x = this->size * 0.25F;
	float cathode_sidesize = this->size * 0.18F;
	float cathode_x = cathode_center_x - electrode_width * 0.5F;
	float cathode_symbol_x = cathode_center_x - cathode_sidesize * 0.5F;
	float cathode_symbol_y = electrode_center_y - this->thickness * 0.5F;

	this->electricity.X = this->thickness;
	this->electricity.Y = battery_y + this->thickness;
	this->electricity.Width = this->size - this->electricity.X * 2.0F;
	this->electricity.Height = battery_height - (this->electricity.Y - battery_y) * 2.0F;
	
	auto battery_region = rounded_rectangle(0.0F, battery_y, this->size, battery_height, corner_radius, corner_radius);
	auto electricity_region = rectangle(this->electricity);

	CanvasGeometry^ battery_parts[] = {
		geometry_subtract(battery_region, electricity_region),
		rectangle(anode_x, 0.0F, electrode_width, battery_y),
		rectangle(anode_hsymbol_x, anode_hsymbol_y, anode_sidesize, this->thickness),
		rectangle(anode_vsymbol_x, anode_vsymbol_y, this->thickness, anode_sidesize),
		rectangle(cathode_x, 0.0F, electrode_width, battery_y),
		rectangle(cathode_symbol_x, cathode_symbol_y, cathode_sidesize, this->thickness),
	};

	this->skeleton = geometry_freeze(geometry_rotate(geometry_union(battery_parts), degrees)); // don't mind, it's Visual Studio's fault
}

void Accumulatorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const AccumulatorStyle style = this->get_style();
	auto color = (style.color != nullptr) ? style.color : default_color;
	auto bgcolor = (style.bgcolor != nullptr) ? style.bgcolor : default_background_color;
	float ex = this->electricity.X + x;
	float ey = this->electricity.Y + y;
	
	ds->FillRectangle(ex, ey, this->electricity.Width, this->electricity.Height, bgcolor);
	ds->DrawCachedGeometry(this->skeleton, x, y, color);
}
