#pragma once

namespace WarGrey::SCADA {
	float viewport_fit_scaling(Windows::Foundation::Size& src_size, float target_width, float target_height);
	float viewport_fit_scaling(float src_width, float src_height, float target_width, float target_height);

	double radians_to_degrees(double degrees);
	float degrees_to_radians(double degrees);
	double degrees_normalize(double degrees);

	double points_angle(Windows::Foundation::Numerics::float2& pt1, Windows::Foundation::Numerics::float2& pt2);
	double points_angle(float x1, float y1, float x2, float y2);

	double arc_length(float radius, double degrees0, double degreesn);

	double circle_perimeter(float radius);
	double ellipse_perimeter(float radiusX, float radiusY);

	void circle_point(float radius, float radians, float* x, float* y);
	void circle_point(float radius, double degrees, float* x, float* y);

	void ellipse_point(float radiusX, float radiusY, float radians, float* x, float* y);
	void ellipse_point(float radiusX, float radiusY, double degrees, float* x, float* y);
	
	void line_point(float x0, float y0, float x1, float y1, double ratio, float* x, float* y);
}
