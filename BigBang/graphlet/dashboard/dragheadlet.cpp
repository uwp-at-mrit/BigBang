#include "graphlet/dashboard/dragheadlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

#include "tongue.hpp"
#include "string.hpp"

#include "measure/vhatchmark.hpp"
#include "measure/hhatchmark.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ drag_default_meter_color = Colours::GhostWhite;
static CanvasSolidColorBrush^ drag_default_head_color = Colours::DimGray;
static CanvasSolidColorBrush^ drag_default_body_color = Colours::Yellow;
static CanvasSolidColorBrush^ drag_default_hatchmark_color = Colours::Silver;

static inline float drag_pt(float height, double depth, double highest, double lowest, float yoff) {
	float percentage = float((highest - depth) / (highest - lowest));
	
	return height * percentage + yoff;
}

/*************************************************************************************************/
DragHeadlet::DragHeadlet(float radius, unsigned int color, float thickness, ICanvasBrush^ bcolor, ICanvasBrush^ hmcolor)
	: radius(std::fabsf(radius)), thickness(thickness), precision(1U), leftward(radius < 0.0F), visor_color(Colours::make(color))
	, body_color(bcolor == nullptr ? drag_default_body_color : bcolor)
	, hatchmark_color(hmcolor == nullptr ? drag_default_hatchmark_color : hmcolor) {}

void DragHeadlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->radius * 2.0F);
}

void DragHeadlet::construct() {
	
}

void DragHeadlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float width = this->radius * 2.0F;
	float height = this->radius * 2.0F;

	ds->DrawRectangle(x, y, width, height, Colours::Crimson);
}
