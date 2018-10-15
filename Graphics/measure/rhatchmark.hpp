#pragma once

namespace WarGrey::SCADA {
	private value struct RHatchMarkMetrics {
		float hatch_width;
		float gap_space;
		float mark_span;
		float em;
	};

	WarGrey::SCADA::RHatchMarkMetrics rhatchmark_metrics(
		float radius, double degrees0, double degreesn, double vmin, double vmax, float thickness,
		unsigned int precision = 1, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rhatchmark(
		float radius, double degrees0, double degreesn, double vmin, double vmax,
		unsigned int step, float thickness = 1.0F, WarGrey::SCADA::RHatchMarkMetrics* metrics = nullptr,
		unsigned int precision = 1, Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);
}
