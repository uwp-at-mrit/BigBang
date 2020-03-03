#pragma once

#include "graphlet/planetlet.hpp"

namespace WarGrey::DTPM {
	private enum class MetricValueType : unsigned int { Flonum, Fixnum, Time, Period, Null };

	private struct MetricValue {
	public:
		MetricValueType type;
		int precision = 0;

		union {
			double flonum;
			long long fixnum;
		} as;
	};

	private struct MetricsStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ metrics_font;

		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ background_color;

		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ slot_color_color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ slot_border_color;

		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ nodatum_color;

		float border_thickness = -1.0F;
	};

	MetricsStyle make_metrics_style(Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ label_font = nullptr,
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ metrics_font = nullptr,
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ background = nullptr,
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ slot_background = nullptr);

	private class IMetricsProvider abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual unsigned int capacity() = 0;

	public:
		virtual Platform::String^ label_ref(unsigned int idx) = 0;
		virtual WarGrey::DTPM::MetricValue value_ref(unsigned int idx) = 0;

	public:
		virtual Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ label_color_ref(unsigned int idx);
		virtual Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ value_color_ref(unsigned int idx);
	};

	private class Metricslet : public WarGrey::SCADA::Planetlet {
	public:
		Metricslet(WarGrey::DTPM::IMetricsProvider* master,
			Platform::String^ name, float width, WarGrey::SCADA::GraphletAnchor anchor, unsigned int slot_count = 0U);

		Metricslet(WarGrey::DTPM::MetricsStyle& style, WarGrey::DTPM::IMetricsProvider* master,
			Platform::String^ name, float width, WarGrey::SCADA::GraphletAnchor anchor, unsigned int slot_count = 0U);
	};
}
