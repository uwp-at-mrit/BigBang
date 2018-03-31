#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "math.hpp"

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
