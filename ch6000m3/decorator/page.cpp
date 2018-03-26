#include "decorator/page.hpp"
#include "graphlet/statuslet.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

PageDecorator::PageDecorator(ICanvasBrush^ brush) : brush(brush) {}
PageDecorator::~PageDecorator() {}

void PageDecorator::draw_after_snip(IGraphlet* self, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) {
	if (x == 0.0) {
		if (y == 0.0) { // statusbar's bottomline
			ds->DrawLine(0, height, width, height, this->brush, 2.0F);
		} else if (dynamic_cast<Statuslinelet*>(self) != nullptr) { // statusline's topline
			ds->DrawLine(0, y, width, y, this->brush, 2.0F);
		}
	}
}
