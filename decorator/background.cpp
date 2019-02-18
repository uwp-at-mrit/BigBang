#include "decorator/background.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

BackgroundDecorator::BackgroundDecorator(unsigned int hex, float tinset, float rinset, float binset, float linset)
	: BackgroundDecorator(Colours::make(hex), tinset, rinset, binset, linset) {}

BackgroundDecorator::BackgroundDecorator(unsigned int color, float inset)
	: BackgroundDecorator(color, inset, inset, inset, inset) {}

BackgroundDecorator::BackgroundDecorator(unsigned int color, float hinset, float vinset)
	: BackgroundDecorator(color, vinset, hinset, vinset, hinset) {}

BackgroundDecorator::BackgroundDecorator(ICanvasBrush^ color, float inset)
	: BackgroundDecorator(color, inset, inset, inset, inset) {}

BackgroundDecorator::BackgroundDecorator(ICanvasBrush^ color, float hinset, float vinset)
	: BackgroundDecorator(color, vinset, hinset, vinset, hinset) {}

BackgroundDecorator::BackgroundDecorator(ICanvasBrush^ color, float tinset, float rinset, float binset, float linset)
	: color(color) {
	this->top_inset = tinset;
	this->right_inset = rinset;
	this->bottom_inset = binset;
	this->left_inset = linset;
}

void BackgroundDecorator::draw_before(CanvasDrawingSession^ ds, float Width, float Height) {
	float width = Width - this->left_inset - this->right_inset;
	float height = Height - this->top_inset - this->bottom_inset;
	
	ds->FillRectangle(this->left_inset, this->top_inset, width, height, this->color);
}
