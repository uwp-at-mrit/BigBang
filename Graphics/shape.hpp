#pragma once

#include "geometry.hpp"

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ blank();
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ paragraph(Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ tl);

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

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ circle(float cx, float cy, float radius);
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ ellipse(float cx, float cy, float radiusX, float radiusY);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rectangle(float width, float height);
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rectangle(float x, float y, float width, float height);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ short_arc(
    float sx, float sy, float ex, float ey, float radiusX, float radiusY,
    float thickness = 1.0F);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ long_arc(
    float sx, float sy, float ex, float ey, float radiusX, float radiusY,
    float thickness = 1.0F);

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

/*************************************************************************************************/
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cylinder_tb_surface(
    float x, float y, float radiusX, float radiusY, float height);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cylinder_tb_surface(
    float radiusX, float radiusY, float height);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cylinder_rl_surface(
    float x, float y, float radiusX, float radiusY, float width);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cylinder_rl_surface(
    float radiusX, float radiusY, float width);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cylinder_lr_surface(
    float x, float y, float radiusX, float radiusY, float width);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cylinder_lr_surface(
    float radiusX, float radiusY, float width);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pyramid_surface(
    float x, float y, float radiusT, float radiusB, float radiusY, float height);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pyramid_surface(
    float radiusT, float radiusB, float radiusY, float height);
