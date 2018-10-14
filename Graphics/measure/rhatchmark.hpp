#pragma once

namespace WarGrey::SCADA {
	private value struct RHatchMarkMetrics {
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

	WarGrey::SCADA::RHatchMarkMetrics rhatchmark_metrics(
		double degrees0, double degreesn, double vmin, double vmax, float thickness,
		unsigned int precision = 1, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rhatchmark(
		float radiusX, float radiusY, double degrees0, double degreesn, double vmin, double vmax,
		unsigned int step, float thickness = 1.0F, WarGrey::SCADA::RHatchMarkMetrics* metrics = nullptr,
		unsigned int precision = 1, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);
}
