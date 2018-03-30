#include <cstdlib>

#include "decorator/decorator.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;

static IPlanetDecorator** make_decorator_list(IPlanetDecorator** src, unsigned int count) {
	auto decorators = (IPlanetDecorator**)calloc(count, sizeof(IPlanetDecorator*));
	
	if (decorators != nullptr) {
		for (unsigned int i = 0; i < count; i++) {
			decorators[i] = src[i];
			decorators[i]->reference();
		}
	}

	return decorators;
}

/*************************************************************************************************/
CompositeDecorator::CompositeDecorator(IPlanetDecorator* first, IPlanetDecorator* second) : count(2) {
	IPlanetDecorator* decorators[] = { first, second };
	this->decorators = make_decorator_list(decorators, 2);
}

CompositeDecorator::CompositeDecorator(IPlanetDecorator** decorators, unsigned int count) : count(count) {
	this->decorators = make_decorator_list(decorators, count);
}

CompositeDecorator::~CompositeDecorator() {
	for (unsigned int i = 0; i < this->count; i++) {
		this->decorators[i]->destroy();
	}

	free(this->decorators);
}

void CompositeDecorator::draw_before(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
	for (unsigned int i = 0; i < this->count; i++) {
		this->decorators[i]->draw_before(master, ds, Width, Height);
	}
}

void CompositeDecorator::draw_after(IPlanet* master, CanvasDrawingSession^ ds, float Width, float Height) {
	for (unsigned int i = 0; i < this->count; i++) {
		this->decorators[i]->draw_after(master, ds, Width, Height);
	}
}

void CompositeDecorator::draw_before_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) {
	for (unsigned int i = 0; i < this->count; i++) {
		this->decorators[i]->draw_before_graphlet(g, ds, x, y, width, height, selected);
	}
}

void CompositeDecorator::draw_after_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool selected) {
	for (unsigned int i = 0; i < this->count; i++) {
		this->decorators[i]->draw_after_graphlet(g, ds, x, y, width, height, selected);
	}
}
