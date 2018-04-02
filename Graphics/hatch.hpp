#pragma once

Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vhatch(
	float range, unsigned char step,
	Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr,
	float* mark_width = nullptr,
	float* mark_y = nullptr,
	float* mark_height = nullptr);
