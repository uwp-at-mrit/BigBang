#include "decorator/background.hpp"

#include "configuration.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

static ICanvasBrush^ bgcolor = nullptr;

BackgroundDecorator::BackgroundDecorator(float tinset, float rinset, float binset, float linset) {
	this->top_inset = tinset;
	this->right_inset = rinset;
	this->bottom_inset = binset;
	this->left_inset = linset;
}

BackgroundDecorator::BackgroundDecorator(float inset) : BackgroundDecorator(inset, inset, inset, inset) {}
BackgroundDecorator::BackgroundDecorator(float hinset, float vinset) : BackgroundDecorator(vinset, hinset, vinset, hinset) {}

BackgroundDecorator::~BackgroundDecorator() {}

void BackgroundDecorator::draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
	float width = Width - this->left_inset - this->right_inset;
	float height = Height - this->top_inset - this->bottom_inset;
	
	if (bgcolor == nullptr) {
		bgcolor = Colours::make(screen_background_color);
	}

	ds->FillRectangle(this->left_inset, this->top_inset, width, height, bgcolor);
}
