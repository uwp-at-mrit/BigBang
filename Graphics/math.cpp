#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "math.hpp"

using namespace Windows::Foundation;

float viewport_fit_scaling(Size& src_size, float target_width, float target_height) {
	return viewport_fit_scaling(src_size.Width, src_size.Height, target_width, target_height);
}

float viewport_fit_scaling(float src_width, float src_height, float target_width, float target_height) {
	return std::fmin(src_width / target_width, src_height / target_height);
}

void circle_point(float radius, double degrees, float* x, float* y) {
	float radians = float(degrees * M_PI / 180.0);

	(*x) = radius * cosf(radians);
	(*y) = radius * sinf(radians);
}

void line_point(float x0, float y0, float x1, float y1, double ratio, float* x, float* y) {
	float flratio = float(ratio);

	(*x) = (x0 - x1) * flratio + x1;
	(*y) = (y0 - y1) * flratio + y1;
}
