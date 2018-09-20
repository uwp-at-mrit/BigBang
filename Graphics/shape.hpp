#pragma once

#include "text.hpp"
#include "geometry.hpp"

namespace WarGrey::SCADA {
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ blank();

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ paragraph(
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ tl,
		WarGrey::SCADA::TextExtent* te = nullptr, bool adjust = true);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ paragraph(
		Platform::String^ text, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr,
		WarGrey::SCADA::TextExtent* te = nullptr, bool adjust = true);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vline(
		float length, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vline(
		float x, float y, float length, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ hline(
		float length, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ hline(
		float x, float y, float length, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ line(
		float ex, float ey, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ line(
		float sx, float sy, float ex, float ey, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ circle(float radius);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ circle(float cx, float cy, float radius);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ ellipse(float radiusX, float radiusY);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ ellipse(float cx, float cy, float radiusX, float radiusY);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ sector(double start_degrees, double end_degrees, float radiusX, float radiusY = 0.0);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ sector(float cx, float cy, double start_degrees, double end_degrees, float radiusX, float radiusY = 0.0);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ masked_sector(double start_degrees, double end_degrees, double ratio, float radiusX, float radiusY = 0.0);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ masked_sector(float cx, float cy, double start_degrees, double end_degrees, double ratio, float radiusX, float radiusY = 0.0);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ segment(double start_degrees, double end_degrees, float radiusX, float radiusY = 0.0);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ segment(float cx, float cy, double start_degrees, double end_degrees, float radiusX, float radiusY = 0.0);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rectangle(float width, float height);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rectangle(float x, float y, float width, float height);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rectangle(Windows::Foundation::Rect& region);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ arc(
		double start_degrees, double end_degrees, float radiusX, float radiusY, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ arc(
		float cx, float cy, double start_degrees, double end_degrees, float radiusX, float radiusY, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ short_arc(
		double start_degrees, double end_degrees, float radiusX, float radiusY, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ short_arc(
		float sx, float sy, float ex, float ey, float radiusX, float radiusY, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ long_arc(
		double start_degrees, double end_degrees, float radiusX, float radiusY, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ long_arc(
		float sx, float sy, float ex, float ey, float radiusX, float radiusY, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ omega(double start_degrees, float radius,
		float thickness = 1.0F, Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr,
		float extent = -1.618F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ omega(float cx, float cy, double start_degrees, float radius,
		float thickness = 1.0F, Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr,
		float extent = -1.618F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ triangle(float x1, float y1, float x2, float y2);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rounded_rectangle(
		float x, float y, float width, float height, float radiusX = -0.25F, float radiusY = -0.25F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rotate_rectangle(float width, float height, double degrees);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rotate_rectangle(
		float x, float y, float width, float height, double degrees);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rotate_rectangle(
		float width, float height, double degrees, float centerX, float centerY);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rotate_rectangle(
		float x, float y, float width, float height, double degrees, float centerX, float centerY);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ double_arrow(
		float x, float y, float length, float arrowhead_size, float spacing = -2.0F, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ double_arrow(
		float length, float arrowhead_size = 4.0F, float spacing = -1.0F, float thickness = 1.0F,
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ stadium(float length, float radius);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ stadium(float x, float y, float length, float radius);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ trapezoid(float ubase, float bbase, float height);
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ trapezoid(float x, float y, float ubase, float bbase, float height);
}
