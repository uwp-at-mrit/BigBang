#include "graphlet/symbol/powerstationlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static PowerStationStatus default_power_station_status = PowerStationStatus::Normal;
static CanvasSolidColorBrush^ default_color = Colours::GhostWhite;

PowerStationStyle WarGrey::SCADA::make_default_power_station_style(PowerStationStatus status) {
	PowerStationStyle s;

	s.color = default_color;

	switch (status) {
	case PowerStationStatus::Breakdown: s.color = Colours::Firebrick; break;
	}

	return s;
}

/*************************************************************************************************/
PowerStationlet::PowerStationlet(float radius, float thickness, double degrees)
	: PowerStationlet(default_power_station_status, radius, thickness, degrees) {
}

PowerStationlet::PowerStationlet(PowerStationStatus default_status, float radius, float thickness, double degrees)
	: ISymbollet(default_status, &make_default_power_station_style, radius, degrees), thickness(thickness) {
	this->ring_radius = radius * 0.618F;
	float r = (this->size - ring_radius - ring_radius - this->thickness) * 0.5F;

	circle_point(r, this->degrees - 90.0, &this->cx1, &this->cy1);
	circle_point(r, this->degrees - 270.0, &this->cx2, &this->cy2);
}

void PowerStationlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const PowerStationStyle style = this->get_style();
	auto color = (style.color != nullptr) ? style.color : default_color;
	float cx = x + this->size * 0.5F;
	float cy = y + this->size * 0.5F;
	
	ds->FillCircle(this->cx1 + cx, this->cy1 + cy, this->ring_radius, Colours::Background);
	ds->FillCircle(this->cx2 + cx, this->cy2 + cy, this->ring_radius, Colours::Background);
	ds->DrawCircle(this->cx1 + cx, this->cy1 + cy, this->ring_radius, color, this->thickness);
	ds->DrawCircle(this->cx2 + cx, this->cy2 + cy, this->ring_radius, color, this->thickness);
}
