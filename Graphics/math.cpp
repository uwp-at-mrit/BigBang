#define _USE_MATH_DEFINES
#include <WindowsNumerics.h>

#include "box.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

inline static float quick_degrees_to_radians(double degrees) {
	return float(degrees * M_PI / 180.0);
}

float WarGrey::SCADA::viewport_fit_scaling(Size& src_size, float target_width, float target_height) {
	return viewport_fit_scaling(src_size.Width, src_size.Height, target_width, target_height);
}

float WarGrey::SCADA::viewport_fit_scaling(float src_width, float src_height, float target_width, float target_height) {
	return std::fmin(src_width / target_width, src_height / target_height);
}

double WarGrey::SCADA::radians_to_degrees(double radians) {
	return (radians / M_PI) * 180.0;
}

float WarGrey::SCADA::degrees_to_radians(double degrees) {
	return quick_degrees_to_radians(degrees);
}

double WarGrey::SCADA::degrees_normalize(double degrees) {
	while (degrees < 0.000) degrees += 360.0;
	while (degrees >= 360.0) degrees -= 360.0;

	return degrees;
}

double WarGrey::SCADA::points_angle(float2& pt1, float2& pt2) {
	return points_angle(pt1.x, pt1.y, pt2.x, pt2.y);
}

double WarGrey::SCADA::points_angle(float x1, float y1, float x2, float y2) {
	return radians_to_degrees(std::atan2(double(y2 - y1), double(x2 - x1)));
}

double WarGrey::SCADA::arc_length(float r, double deg0, double degn) {
	double theta = std::abs(degn - deg0);

	return (theta >= 360.0) ? circle_perimeter(r) :  (M_PI * r * theta / 180.0);
}

double WarGrey::SCADA::circle_perimeter(float r) {
	return 2 * M_PI * r;
}

double WarGrey::SCADA::ellipse_perimeter(float a, float b) {
	return (a == b)
		? 2.0 * M_PI * a
		: M_PI * (3.0F * (a + b) - std::sqrt((3.0 * a + b) * (a + 3.0 * b)));
}

void WarGrey::SCADA::circle_point(float radius, double degrees, float* x, float* y) {
	circle_point(radius, quick_degrees_to_radians(degrees), x, y);
}

void WarGrey::SCADA::circle_point(float radius, float radians, float* x, float* y) {
	SET_BOX(x, radius * cosf(radians));
	SET_BOX(y, radius * sinf(radians));
}

void WarGrey::SCADA::ellipse_point(float radiusX, float radiusY, double degrees, float* x, float* y) {
	ellipse_point(radiusX, radiusY, quick_degrees_to_radians(degrees), x, y);
}

void WarGrey::SCADA::ellipse_point(float radiusX, float radiusY, float radians, float* x, float* y) {
	SET_BOX(x, radiusX * cosf(radians));
	SET_BOX(y, radiusY * sinf(radians));
}

void WarGrey::SCADA::line_point(float x0, float y0, float x1, float y1, double ratio, float* x, float* y) {
	float flratio = float(ratio);

	SET_BOX(x, (x0 - x1) * flratio + x1);
	SET_BOX(y, (y0 - y1) * flratio + y1);
}
