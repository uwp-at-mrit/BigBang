#include "configuration.hpp"
#include "system.hpp"

float application_fit_size(float src) {
	static float backscaling = std::fmin(system_resolution_fit_scaling(resolution_width, resolution_height), 1.0F);

	return src * backscaling;
}
