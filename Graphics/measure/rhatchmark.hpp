#pragma once

namespace WarGrey::SCADA {
	private value struct RHatchMarkMetrics {
		unsigned int span;
		float ring_radius;
		float hatch_width;
		float gap_space;
		float em;
		float ch;

		float label_lx;
		float label_rx;
		float label_ty;
		float label_by;

		float arc_sx;
		float arc_sy;
		float arc_ex;
		float arc_ey;
	};

	WarGrey::SCADA::RHatchMarkMetrics rhatchmark_metrics(
		float radius, double vmin, double vmax, float thickness, unsigned int precision = 0U,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rhatchmark(
		float radius, double degrees0, double degreesn, double vmin, double vmax,
		unsigned int step, float thickness = 1.0F, WarGrey::SCADA::RHatchMarkMetrics* metrics = nullptr,
		unsigned int precision = 0U, bool no_short = false,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);
}
