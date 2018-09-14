#include "graphlet/symbol/circuit/powerstationlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
PowerStationlet::PowerStationlet(float radius, float thickness, double degrees)
	: PowerStationlet(PowerStationStatus::Normal, radius, thickness, degrees) {
}

PowerStationlet::PowerStationlet(PowerStationStatus default_status, float radius, float thickness, double degrees)
	: ISymbollet(default_status, radius, degrees), thickness(thickness) {
	this->ring_radius = radius * 0.618F;
	float r = (this->width - ring_radius - ring_radius - this->thickness) * 0.5F;

	circle_point(r, this->degrees - 90.0, &this->cx1, &this->cy1);
	circle_point(r, this->degrees - 270.0, &this->cx2, &this->cy2);
}

void PowerStationlet::prepare_style(PowerStationStatus status, PowerStationStyle& s) {
	switch (status) {
	case PowerStationStatus::Breakdown: CAS_SLOT(s.color, Colours::Firebrick); break;
	}

	CAS_SLOT(s.color, Colours::GhostWhite);
}

void PowerStationlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const PowerStationStyle style = this->get_style();
	float cx = x + this->width * 0.5F;
	float cy = y + this->height * 0.5F;
	
	ds->FillCircle(this->cx1 + cx, this->cy1 + cy, this->ring_radius, Colours::Background);
	ds->FillCircle(this->cx2 + cx, this->cy2 + cy, this->ring_radius, Colours::Background);
	ds->DrawCircle(this->cx1 + cx, this->cy1 + cy, this->ring_radius, style.color, this->thickness);
	ds->DrawCircle(this->cx2 + cx, this->cy2 + cy, this->ring_radius, style.color, this->thickness);
}
