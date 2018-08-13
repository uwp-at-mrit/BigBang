#pragma once

namespace WarGrey::SCADA {
	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatch(
		float height, float vmin, float vmax, unsigned int step, float thickness,
		Windows::Foundation::Rect* hatch_box = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatch(
		float height, float vmin, float vmax, unsigned int step, float thickness,
		Windows::Foundation::Rect* hatch_box = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);
}
