#pragma once

#include "graphlet/planetlet.hpp"

namespace WarGrey::DTPM {
	private struct MetricsStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ metrics_font;

		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ background;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ slot_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ slot_border;
	};

	MetricsStyle make_metrics_style(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ metrics_font = nullptr,
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ background = nullptr,
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ slot_background = nullptr);

	private class Metricslet abstract : public WarGrey::SCADA::Planetlet {
	public:
		Metricslet(Platform::String^ name, float width, WarGrey::SCADA::GraphletAnchor anchor, unsigned int slot_count = 0U);
		Metricslet(WarGrey::DTPM::MetricsStyle& style, Platform::String^ name, float width, WarGrey::SCADA::GraphletAnchor anchor, unsigned int slot_count = 0U);
	};
}
