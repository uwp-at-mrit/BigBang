#pragma once

namespace WarGrey::SCADA {
	float vhatchmark_width(float vmin, float vmax, float thickness,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr,
		float* hatch_width = nullptr, float* ch = nullptr, float* em = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatch(
		float width, float height, unsigned int step, float thickness);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatch(
		float width, float height, unsigned int step, float thickness);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatchmark(
		float height, float vmin, float vmax, unsigned int step, float thickness,
		Windows::Foundation::Rect* hatch_box = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatchmark(
		float height, float vmin, float vmax, unsigned int step, float thickness,
		Windows::Foundation::Rect* hatch_box = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);
}
