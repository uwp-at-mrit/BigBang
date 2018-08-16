#pragma once

namespace WarGrey::SCADA {
	private value struct VHatchMarkMetrics {
		unsigned int span;
		float mark_width;
		float tspace;
		float ch;
		float em;

		float width;
		float hatch_x;
		float hatch_y;
		float hatch_width;
		float hatch_height;
	};

	WarGrey::SCADA::VHatchMarkMetrics vhatchmark_metrics(float vmin, float vmax, float thickness,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	WarGrey::SCADA::VHatchMarkMetrics vhatchmark_metrics(Platform::String^ marks[], size_t count, float thickness,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatch(
		float width, float height, unsigned int step, float thickness = 1.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatch(
		float width, float height, unsigned int step, float thickness = 1.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatch(
		float width, float height, unsigned int weights[], size_t count, unsigned int base,
		float thickness = 1.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatch(
		float width, float height, unsigned int weights[], size_t count, unsigned int base,
		float thickness = 1.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatchmark(
		float height, float vmin, float vmax, unsigned int step,
		float thickness = 1.0F, WarGrey::SCADA::VHatchMarkMetrics* metrics = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatchmark(
		float height, Platform::String^ marks[], unsigned int weights[], size_t count, unsigned int base,
		float thickness = 1.0F, WarGrey::SCADA::VHatchMarkMetrics* metrics = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatchmark(
		float height, float vmin, float vmax, unsigned int step,
		float thickness = 1.0F, WarGrey::SCADA::VHatchMarkMetrics* metrics = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatchmark(
		float height, Platform::String^ marks[], unsigned int weights[], size_t count, unsigned int base,
		float thickness = 1.0F, WarGrey::SCADA::VHatchMarkMetrics* metrics = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);
}
