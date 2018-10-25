#pragma once

namespace WarGrey::SCADA {
	private value struct VHatchMarkMetrics {
		unsigned int span;
		float mark_width;
		float gap_space;
		float top_space;
		float ch;
		float em;

		float width;
		float hatch_x;
		float hatch_y;
		float hatch_width;
		float hatch_height;
	};

	WarGrey::SCADA::VHatchMarkMetrics vhatchmark_metrics(double vmin, double vmax, float thickness,
		unsigned int precision = 0, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	WarGrey::SCADA::VHatchMarkMetrics vhatchmark_metrics(Platform::String^ marks[], size_t count, float thickness,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatch(
		float width, float height, unsigned int step, float thickness = 1.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatch(
		float width, float height, unsigned int step, float thickness = 1.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatch(
		float width, float height, double weights[], size_t count,
		float thickness = 1.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatch(
		float width, float height, double weights[], size_t count,
		float thickness = 1.0F);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vhatchmark(
		float height, double vmin, double vmax, unsigned int step,
		float thickness = 1.0F, WarGrey::SCADA::VHatchMarkMetrics* metrics = nullptr,
		unsigned int precision = 0, bool no_short = false,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vhatchmark(
		float height, Platform::String^ marks[], double weights[], size_t count,
		float thickness = 1.0F, WarGrey::SCADA::VHatchMarkMetrics* metrics = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatchmark(
		float height, double vmin, double vmax, unsigned int step,
		float thickness = 1.0F, WarGrey::SCADA::VHatchMarkMetrics* metrics = nullptr,
		unsigned int precision = 0, bool no_short = false,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vlhatchmark(
		float height, Platform::String^ marks[], double weights[], size_t count,
		float thickness = 1.0F, WarGrey::SCADA::VHatchMarkMetrics* metrics = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatchmark(
		float height, double vmin, double vmax, unsigned int step,
		float thickness = 1.0F, WarGrey::SCADA::VHatchMarkMetrics* metrics = nullptr,
		unsigned int precision = 0, bool no_short = false,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vrhatchmark(
		float height, Platform::String^ marks[], double weights[], size_t count,
		float thickness = 1.0F, WarGrey::SCADA::VHatchMarkMetrics* metrics = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);
}
