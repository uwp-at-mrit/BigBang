#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

void draw_invalid_bitmap(Platform::String^ hint, CanvasDrawingSession^ ds
	, float x, float y, float width, float height, float thickness
	, ICanvasBrush^ color, CanvasTextFormat^ font) {
	float x0 = x + thickness * 0.5F;
	float y0 = y + thickness * 0.5F;
	float xn = x0 + width - thickness;
	float yn = y0 + height - thickness;

	ds->DrawRectangle(x0, y0, xn - x0, yn - y0, color, thickness);
	ds->DrawLine(x0, y0, xn, yn, color, thickness);
	ds->DrawLine(x0, yn, xn, y0, color, thickness);
	ds->DrawText(hint, x + thickness, y, color, font);
}