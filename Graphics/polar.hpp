#pragma once

#include "geometry.hpp"

namespace WarGrey::SCADA {
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_axis(float radius, double degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_axis(float radiusX, float radiusY, double degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_pole(float radius, double degrees, float pt_radius);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_pole(float radius, float radiusY, double degrees, float pt_radius);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_line(float radius, double start_degrees, double end_degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_line(float radiusX, float radiusY, double start_degrees, double end_degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_arrowhead(float radius, double degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_arrowhead(float radiusX, float radiusY, double degrees);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_triangle(float radius, double degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_triangle(float radiusX, float radiusY, double degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_masked_triangle(float radius, double degrees, double height_ratio);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_masked_triangle(float radiusX, float radiusY, double degrees, double height_ratio);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_sandglass(float radius, double degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_sandglass(float radiusX, float radiuxY, double degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_masked_sandglass(float radius, double degrees, double height_ratio);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_masked_sandglass(float radiusX, float radiusY, double degrees, double height_ratio);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_rectangle(float radius, double alpha_degrees, double rotation_degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_rectangle(float radiusX, float radiusY, double alpha_degrees, double rotation_degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_masked_rectangle(float radius, double alpha_degrees, double rotation_degrees, double height_ratio);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_masked_rectangle(float radiusX, float radiusY, double alpha_degrees, double rotation_degrees, double height_ratio);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_trapezoid(float radius, float ubase, double rotation_degrees, float* height = nullptr);

	// TODO
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_wrench(float radius, double alpha_degrees, double rotation_degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ polar_wrench(float radiusX, float radiusY, double alpha_degrees, double rotation_degrees);
}