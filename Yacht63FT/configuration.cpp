#include "configuration.hpp"
#include "system.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

float application_fit_size(float src) {
	static float backscaling = std::fmin(viewport_fit_scaling(system_screen_size(), screen_width, screen_height), 1.0F);

	return src * backscaling;
}
