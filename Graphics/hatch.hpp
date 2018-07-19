#pragma once

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vhatch(
	float height, float vmin, float vmax, unsigned int step, float thickness, 
	Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr,
	float* mark_width = nullptr,
	float* mark_y = nullptr,
	float* mark_height = nullptr);
