#pragma once

namespace WarGrey::SCADA {
	private value struct HHatchMarkMetrics {
		float gap_space;
		float top_space;
		float ch;
		float em;

		float height;
		float hatch_x;
		float hatch_y;
		float hatch_width;
		float hatch_height;
		float hatch_right_space;
	};

	WarGrey::SCADA::HHatchMarkMetrics hhatchmark_metrics(double vmin, double vmax, float thickness,
		unsigned int precision = 0, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ hthatchmark(
		float width, double vmin, double vmax, unsigned int step,
		float thickness = 1.0F, WarGrey::SCADA::HHatchMarkMetrics* metrics = nullptr,
		unsigned int precision = 0,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ hbhatchmark(
		float width, double vmin, double vmax, unsigned int step,
		float thickness = 1.0F, WarGrey::SCADA::HHatchMarkMetrics* metrics = nullptr,
		unsigned int precision = 0,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);
}
