#include "graphlet/symbol/circuit/solarpanellet.hpp"

#include "math.hpp"
#include "text.hpp"
#include "shape.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;
using namespace Microsoft::Graphics::Canvas::Brushes;

static SolarPanelStatus default_solarpanel_status = SolarPanelStatus::Normal;
static CanvasSolidColorBrush^ default_color = Colours::GhostWhite;
static CanvasSolidColorBrush^ default_background_color = Colours::CornflowerBlue;

SolarPanelStyle WarGrey::SCADA::make_default_solarpanel_style(SolarPanelStatus status) {
	SolarPanelStyle s;

	s.color = default_color;
	s.bgcolor = default_background_color;

	switch (status) {
	case SolarPanelStatus::Breakdown: s.color = Colours::Firebrick; break;
	}

	return s;
}

/*************************************************************************************************/
SolarPanellet::SolarPanellet(float radius, float thickness, double degrees)
	: SolarPanellet(default_solarpanel_status, radius, thickness, degrees) {}

SolarPanellet::SolarPanellet(SolarPanelStatus default_status, float radius, float thickness, double degrees)
	: ISymbollet(default_status, &make_default_solarpanel_style, radius, degrees), thickness(thickness) {}

void SolarPanellet::construct() {
	CanvasGeometry^ col[4];
	CanvasGeometry^ row[3];
	float length = this->size - this->thickness * 2.0F;
	float wgap = length / float(sizeof(col) / sizeof(CanvasGeometry^) + 1);
	float hgap = length / float(sizeof(row) / sizeof(CanvasGeometry^) + 1);

	for (int i = 0; i < sizeof(col) / sizeof(CanvasGeometry^); i++) {
		col[i] = vline(this->thickness + wgap * float(i + 1), this->thickness, length);
	}

	for (int i = 0; i < sizeof(row) / sizeof(CanvasGeometry^); i++) {
		row[i] = hline(this->thickness, this->thickness + hgap * float(i + 1), length);
	}

	// Don't mind, it Visual Studio's fault.
	this->body = geometry_freeze(geometry_union(geometry_union(col), geometry_union(row)));
}

void SolarPanellet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	const SolarPanelStyle style = this->get_style();
	ICanvasBrush^ color = (style.color != nullptr) ? style.color : default_color;
	ICanvasBrush^ bgcolor = (style.bgcolor != nullptr) ? style.bgcolor : default_background_color;
	float offset = this->thickness * 0.5F;
	float length = this->size - this->thickness;

	ds->FillRectangle(x + offset, y + offset, length, length, bgcolor);
	ds->DrawCachedGeometry(this->body, x, y, color);
	ds->DrawRectangle(x + offset, y + offset, length, length, color, this->thickness);
}
