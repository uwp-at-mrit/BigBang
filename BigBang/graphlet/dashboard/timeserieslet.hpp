#pragma once

#include "graphlet/primitive.hpp"

#include "time.hpp"
#include "tongue.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	typedef Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ (*lookup_line_color)(unsigned int idx);

	struct TimeSeriesLine;

	private enum class TimeSeriesState { Realtime, History, _ };

	private struct TimeSeries {
		long long start;
		long long span;
		unsigned int step;
	};

	WarGrey::SCADA::TimeSeries make_minute_series(unsigned int count = 1U, unsigned int step = 5);
	WarGrey::SCADA::TimeSeries make_hour_series(unsigned int count = 1U, unsigned int step = 5);
	WarGrey::SCADA::TimeSeries make_today_series(unsigned int step = 11);

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
		float maximum_fx = -1.0F;
	};

	private class ITimeSerieslet abstract : public WarGrey::SCADA::IStatelet<WarGrey::SCADA::TimeSeriesState, WarGrey::SCADA::TimeSeriesStyle> {
	public:
		virtual ~ITimeSerieslet() noexcept;

		ITimeSerieslet(double vmin, double vmax, WarGrey::SCADA::TimeSeries& ts, unsigned int n,
			float width, float height, unsigned int step, unsigned int precision, long long history_s);

	public:
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) override;
		void own_caret(bool yes) override;
		
	protected:
		void set_value(unsigned int idx, double value);
		void set_values(double* values);

	protected:
		void prepare_style(WarGrey::SCADA::TimeSeriesState state, WarGrey::SCADA::TimeSeriesStyle& style) override;
		void on_state_changed(WarGrey::SCADA::TimeSeriesState state) override;

	protected:
		void construct_line(unsigned int idx, Platform::String^ name);
		void enable_closed_line(unsigned int idx, bool on_off);

	private:
		void update_time_series(long long next_start);
		void update_vertical_axes(WarGrey::SCADA::TimeSeriesStyle& style);
		void update_horizontal_axes(WarGrey::SCADA::TimeSeriesStyle& style);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vaxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ haxes;

	private:
		WarGrey::SCADA::TimeSeriesLine* lines;
		unsigned int count;

	private:
		float width;
		float height;

	private:
		WarGrey::SCADA::TimeSeries realtime;
		WarGrey::SCADA::TimeSeries history;
		unsigned int vertical_step;
		unsigned int precision;
		long long history_max;

	private:
		double vmin;
		double vmax;
	};

	template<typename Name>
	private class TimeSerieslet : public WarGrey::SCADA::ITimeSerieslet {
	public:
		TimeSerieslet(Platform::String^ tongue, double range, float width, float height = 0.0F
			, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: TimeSerieslet(tongue, 0.0, range, width, height, step, precision, history_s) {}

		TimeSerieslet(Platform::String^ tongue, double range, WarGrey::SCADA::TimeSeries& ts
			, float width, float height = 0.0F, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: TimeSerieslet(tongue, 0.0, range, ts, width, height, step, precision, history_s) {}

		TimeSerieslet(Platform::String^ tongue, double vmin, double vmax, float width, float height = 0.0F
			, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: TimeSerieslet(tongue, vmin, vmax, WarGrey::SCADA::make_today_series(), width, height, step, precision, history_s) {}

		TimeSerieslet(Platform::String^ tongue, double vmin, double vmax, WarGrey::SCADA::TimeSeries& ts,
			float width, float height = 0.0F, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: ITimeSerieslet(vmin, vmax, ts, _N(Name), width, height, step, precision, history_s), tongue(tongue) {}

	public:
		void construct() override {
			for (unsigned int idx = 0; idx < _N(Name); idx++) {
				this->construct_line(idx, speak(_E(Name, idx), this->tongue));
			}
		}

	public:
		void set_value(Name slot, double value) {
			ITimeSerieslet::set_value(_I(slot), value);
		}

		void set_values(double* values) {
			ITimeSerieslet::set_values(values);
		}

		void enable_closed_line(Name slot, bool on_off) {
			ITimeSerieslet::enable_closed_line(_I(slot), on_off);
		}

	private:
		Platform::String^ tongue;
	};
}
