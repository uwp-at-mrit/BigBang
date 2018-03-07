#include "decorator/decorator.hpp"

#include "paint.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;

inline static void do_reference(IPlanetDecorator** decorators, unsigned int count) {
	for (unsigned int i = 0; i < count; i++) {
		decorators[i]->reference();
	}
}

/*************************************************************************************************/
ComposeDecorator::ComposeDecorator(IPlanetDecorator* first, IPlanetDecorator* second) {

}

ComposeDecorator::ComposeDecorator(const IPlanetDecorator** decorators, unsigned int count) {

}

ComposeDecorator::~ComposeDecorator() {
	for (unsigned int i = 0; i < this->count; i++) {
		this->decorators[i]->destroy();
	}
}

void ComposeDecorator::draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
	for (unsigned int i = 0; i < this->count; i++) {
		this->decorators[i]->draw_before(master, ds, Width, Height);
	}
}

void ComposeDecorator::draw_after(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
	for (unsigned int i = 0; i < this->count; i++) {
		this->decorators[i]->draw_after(master, ds, Width, Height);
	}
}

void ComposeDecorator::draw_before_snip(ISnip* snip, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) {
	for (unsigned int i = 0; i < this->count; i++) {
		this->decorators[i]->draw_before_snip(snip, ds, x, y, width, height, selected);
	}
}

void ComposeDecorator::draw_after_snip(ISnip* snip, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) {
	for (unsigned int i = 0; i < this->count; i++) {
		this->decorators[i]->draw_after_snip(snip, ds, x, y, width, height, selected);
	}
}
