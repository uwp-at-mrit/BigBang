#include "configuration.hpp"
#include "system.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

float screen_to_application_size(float size) {
	static const float backscaling = std::fmin(viewport_fit_scaling(system_screen_size(), screen_width, screen_height), 1.0F);

	return size * backscaling;
}

float design_to_application_width(float size) {
	return screen_to_application_size(size / 1920.0F * screen_width);
}

float design_to_application_height(float size) {
	return screen_to_application_size(size / 1200.0F * screen_height);
}
