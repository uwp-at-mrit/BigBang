#pragma once

#include "geometry.hpp"

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
