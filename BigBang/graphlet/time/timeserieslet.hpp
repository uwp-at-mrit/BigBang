#pragma once

#include "graphlet/primitive.hpp"

#include "time.hpp"
#include "tongue.hpp"
#include "object.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	typedef Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush ^ (*lookup_line_color)(unsigned int idx);

	class TimeSeriesLine;

	private enum class TimeSeriesState { Realtime, History, _ };

	private struct TimeSeries {
		long long start;
		long long span;
		unsigned int step;
	};

	WarGrey::SCADA::TimeSeries make_minute_series(unsigned int count = 1U, unsigned int step = 6);
	WarGrey::SCADA::TimeSeries make_hour_series(unsigned int count = 1U, unsigned int step = 6);
	WarGrey::SCADA::TimeSeries make_today_series(unsigned int step = 12);

	Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ lookup_default_light_color(unsigned int idx);
	Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ lookup_default_dark_color(unsigned int idx);

	private struct TimeSeriesStyle {
		WarGrey::SCADA::lookup_line_color lookup_color = nullptr;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ legend_font;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ haxes_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ vaxes_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ selected_color;

		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ haxes_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ vaxes_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ lines_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ selected_style;

		float lines_thickness = -1.0F;
		float border_thickness = -1.0F;
		float haxes_thickness = -1.0F;
		float vaxes_thickness = -1.0F;
		float selected_thickness = -1.0F;

		float legend_fx = -1.0F;
		float maximum_fx = -1.0F;
	};

	/************************************************************************************************/
	private class ITimeSeriesDataReceiver abstract {
	public:
		virtual void begin_maniplation_sequence() {}
		virtual void on_datum_values(long long timepoint, double* values, unsigned int n) = 0;
		virtual void end_maniplation_sequence() {}

	public:
		virtual void on_maniplation_complete(long long open_s, long long close_s) {}
	};

	private class ITimeSeriesDataSource abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual bool ready() = 0;
		virtual bool loading() = 0;

	public:
		virtual void load(WarGrey::SCADA::ITimeSeriesDataReceiver* receiver, long long open_s, long long close_s) = 0;
		virtual void save(long long timepoint, double* values, unsigned int n) = 0;

	protected:
		~ITimeSeriesDataSource() noexcept {}
	};

	/************************************************************************************************/
	private class ITimeSerieslet abstract
		: public WarGrey::SCADA::IStatelet<WarGrey::SCADA::TimeSeriesState, WarGrey::SCADA::TimeSeriesStyle>
		, public WarGrey::SCADA::ITimeSeriesDataReceiver {
	public:
		virtual ~ITimeSerieslet() noexcept;

		ITimeSerieslet(WarGrey::SCADA::ITimeSeriesDataSource* src,
			double vmin, double vmax, WarGrey::SCADA::TimeSeries& ts, unsigned int n,
			float width, float height, unsigned int step, unsigned int precision, long long history_s);

	public:
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		bool on_key(Windows::System::VirtualKey key, bool screen_keyboard) override;
		void on_tap(float local_x, float local_y) override;
		void own_caret(bool yes) override;

	public:
		void on_datum_values(long long timepoint, double* values, unsigned int n) override;
		void on_maniplation_complete(long long open_s, long long close_s) override;

	protected:
		void set_value(unsigned int idx, double value);
		void set_values(double* values, bool persistent = true);

	protected:
		void prepare_style(WarGrey::SCADA::TimeSeriesState state, WarGrey::SCADA::TimeSeriesStyle& style) override;
		void apply_style(WarGrey::SCADA::TimeSeriesStyle& style) override;

	protected:
		void construct_line(unsigned int idx, Platform::String^ name);
		void close_line(unsigned int idx, double alpha);
		void hide_line(unsigned int idx, bool yes_no);

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
		double vmin;
		double vmax;

	private:
		WarGrey::SCADA::TimeSeries realtime;
		WarGrey::SCADA::TimeSeries history;
		unsigned int vertical_step;
		unsigned int precision;
		long long history_max;
		float selected_x;

	private:
		WarGrey::SCADA::ITimeSeriesDataSource* data_source;
		long long next_loading_timepoint;
	};

	template<typename Name>
	private class TimeSerieslet : public WarGrey::SCADA::ITimeSerieslet {
	public:
		TimeSerieslet(Platform::String^ tongue, double range, float width, float height = 0.0F
			, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: TimeSerieslet(tongue, nullptr, range, width, height, step, precision, history_s) {}

		TimeSerieslet(Platform::String^ tongue, WarGrey::SCADA::ITimeSeriesDataSource* src
			, double range, float width, float height = 0.0F
			, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: TimeSerieslet(tongue, src, 0.0, range, width, height, step, precision, history_s) {}

		TimeSerieslet(Platform::String^ tongue, double range, WarGrey::SCADA::TimeSeries& ts
			, float width, float height = 0.0F, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: TimeSerieslet(tongue, nullptr, range, ts, width, height, step, precision, history_s) {}

		TimeSerieslet(Platform::String^ tongue, WarGrey::SCADA::ITimeSeriesDataSource* src
			, double range, WarGrey::SCADA::TimeSeries& ts, float width, float height = 0.0F
			, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: TimeSerieslet(tongue, src, 0.0, range, ts, width, height, step, precision, history_s) {}

		TimeSerieslet(Platform::String^ tongue, double vmin, double vmax, float width, float height = 0.0F
			, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: TimeSerieslet(tongue, nullptr, vmin, vmax, WarGrey::SCADA::make_today_series(),
				width, height, step, precision, history_s) {}

		TimeSerieslet(Platform::String^ tongue, WarGrey::SCADA::ITimeSeriesDataSource* src
			, double vmin, double vmax, float width, float height = 0.0F
			, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: TimeSerieslet(tongue, src, vmin, vmax, WarGrey::SCADA::make_today_series(),
				width, height, step, precision, history_s) {}

		TimeSerieslet(Platform::String^ tongue, double vmin, double vmax, WarGrey::SCADA::TimeSeries& ts,
			float width, float height = 0.0F, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: TimeSerieslet(tongue, nullptr, vmin, vmax, ts, width, height, step, precision, history_s) {}

		TimeSerieslet(Platform::String^ tongue, WarGrey::SCADA::ITimeSeriesDataSource* src
			, double vmin, double vmax, WarGrey::SCADA::TimeSeries& ts, float width, float height = 0.0F
			, unsigned int step = 0U, unsigned int precision = 2U, long long history_s = day_span_s)
			: ITimeSerieslet(src, vmin, vmax, ts, _N(Name), width, height, step, precision, history_s), tongue(tongue) {}

	public:
		void construct() override {
			for (unsigned int idx = 0; idx < _N(Name); idx++) {
				this->construct_line(idx, speak(_E(Name, idx), this->tongue));
			}
		}

	public:
		using ITimeSerieslet::set_value;
		using ITimeSerieslet::set_values;

		void set_value(Name slot, double value) {
			ITimeSerieslet::set_value(_I(slot), value);
		}

		void set_values(double* values, bool persistent = true) {
			ITimeSerieslet::set_values(values, persistent);
		}

		void close_line(Name slot, double alpha) {
			ITimeSerieslet::close_line(_I(slot), alpha);
		}

		void hide_line(Name slot, bool yes_no) {
			ITimeSerieslet::hide_line(_I(slot), yes_no);
		}

	private:
		Platform::String^ tongue;
	};
}
