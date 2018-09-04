#pragma once

#include "graphlet/primitive.hpp"

#include "tongue.hpp"
#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	typedef Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ (*lookup_line_color)(unsigned int idx);

	private enum class TimeSeriesStatus { Realtime, Manual, _ };

	private struct TimeSeries {
		long long start;
		long long span;
		unsigned int step;
	};

	TimeSeries make_today_series(unsigned int step = 11);

	Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ lookup_default_light_color(unsigned int idx);
	Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ lookup_default_dark_color(unsigned int idx);

	private struct TimeSeriesStyle {
		WarGrey::SCADA::lookup_line_color lookup_color;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ legend_font;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ haxes_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ vaxes_color;

		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ haxes_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ vaxes_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ lines_style;

		float lines_thickness = -1.0F;
		float border_thickness = -1.0F;
		float haxes_thickness = -1.0F;
		float vaxes_thickness = -1.0F;

		float legend_fx = -1.0F;
	};

	private class ITimeSerieslet abstract
		: public WarGrey::SCADA::IStatuslet<WarGrey::SCADA::TimeSeriesStatus, WarGrey::SCADA::TimeSeriesStyle> {
	public:
		ITimeSerieslet(double vmin, double vmax, WarGrey::SCADA::TimeSeries& ts, unsigned int n,
			float width, float height, unsigned int step, unsigned int precision);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::TimeSeriesStatus status, WarGrey::SCADA::TimeSeriesStyle& style) override;
		void on_status_changed(WarGrey::SCADA::TimeSeriesStatus status) override;

	protected:
		void construct_line(unsigned int idx, Platform::String^ name);

	private:
		void update_vertical_axes(WarGrey::SCADA::TimeSeriesStyle& style);
		void update_horizontal_axes(WarGrey::SCADA::TimeSeriesStyle& style);
		void update_legend(unsigned int idx, WarGrey::SCADA::TimeSeriesStyle& style);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vaxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ haxes;

	private:
		Platform::Array<Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^>^ colors;
		Platform::Array<Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^>^ lines;
		Platform::Array<Microsoft::Graphics::Canvas::Text::CanvasTextLayout^>^ legends;
		Platform::Array<Platform::String^>^ names;
		Platform::Array<double>^ values;
		Platform::Array<float>^ xs;
		Platform::Array<float>^ ys;

	private:
		float width;
		float height;

	private:
		WarGrey::SCADA::TimeSeries& series;
		unsigned int step;
		unsigned int precision;

	private:
		double vmin;
		double vmax;
	};

	template<typename Name>
	private class TimeSerieslet : public WarGrey::SCADA::ITimeSerieslet {
	public:
		TimeSerieslet(Platform::String^ tongue, double range, float width, float height = 0.0F
			, unsigned int step = 0U, unsigned int precision = 2U)
			: TimeSerieslet(tongue, 0.0, range, width, height, step, precision) {}

		TimeSerieslet(Platform::String^ tongue, double range, WarGrey::SCADA::TimeSeries& ts
			, float width, float height = 0.0F, unsigned int step = 0U, unsigned int precision = 2U)
			: TimeSerieslet(tongue, 0.0, range, ts, width, height, step, precision) {}

		TimeSerieslet(Platform::String^ tongue, double vmin, double vmax, float width, float height = 0.0F
			, unsigned int step = 0U, unsigned int precision = 2U)
			: TimeSerieslet(tongue, vmin, vmax, WarGrey::SCADA::make_today_series(), width, height, step, precision) {}

		TimeSerieslet(Platform::String^ tongue, double vmin, double vmax, WarGrey::SCADA::TimeSeries& ts,
			float width, float height = 0.0F, unsigned int step = 0U, unsigned int precision = 2U)
			: ITimeSerieslet(vmin, vmax, ts, _N(Name), width, height, step, precision), tongue(tongue) {}

	public:
		void construct() override {
			for (unsigned int idx = 0; idx < _N(Name); idx++) {
				this->construct_line(idx, speak(_E(Name, idx), this->tongue));
			}
		}

	private:
		Platform::String^ tongue;
	};
}
