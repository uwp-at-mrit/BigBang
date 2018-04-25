#include "configuration.hpp"
#include "system.hpp"
#include "math.hpp"

float application_fit_size(float src) {
	static float backscaling = std::fmin(viewport_fit_scaling(system_screen_size(), resolution_width, resolution_height), 1.0F);

	return src * backscaling;
}
