#include <cstdlib>

#include "planet.hpp"
#include "decorator/decorator.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::GYDM;

using namespace Microsoft::Graphics::Canvas;

/*************************************************************************************************/
Syslog* IPlanetDecorator::get_logger() {
	Syslog* logger = default_logger();

	if (this->master != nullptr) {
		logger = this->master->get_logger();
	}

	return logger;
}

void IPlanetDecorator::set_active_planet(IPlanet* master) {
	bool changed = (this->master != master);

	this->master = master;

	if (changed) {
		this->on_active_planet_changed(this->master);
	}
}

void IPlanetDecorator::fill_graphlets_boundary(float* x, float* y, float* width, float* height) {
	if (this->master != nullptr) {
		this->master->fill_graphlets_boundary(x, y, width, height);
	}
}

float IPlanetDecorator::actual_width() {
	float width = 0.0F;

	if (this->master != nullptr) {
		width = this->master->actual_width();
	}

	return width;
}

float IPlanetDecorator::actual_height() {
	float height = 0.0F;

	if (this->master != nullptr) {
		height = this->master->actual_height();
	}

	return height;
}

float IPlanetDecorator::sketch_to_application_width(float sketch_width) {
	float width = sketch_width;

	if (this->master != nullptr) {
		this->master->sketch_to_application_width(sketch_width);
	}

	return width;
}

float IPlanetDecorator::sketch_to_application_height(float sketch_height) {
	float height = sketch_height;

	if (this->master != nullptr) {
		this->master->sketch_to_application_width(sketch_height);
	}

	return height;
}
