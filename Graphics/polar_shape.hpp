#pragma once

#include "geometry.hpp"

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ triangle(float radius, double degrees);
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ masked_triangle(float raidus, double degrees, double height_ratio);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ sandglass(float radius, double degrees);
Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ masked_sandglass(float raidus, double degrees, double height_ratio);

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rectangle(float radius, double degrees);
