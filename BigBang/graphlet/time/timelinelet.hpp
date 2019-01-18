#pragma once
#pragma warning (disable: 4250)

#include <deque>

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "timer.hxx"

namespace WarGrey::SCADA {
	private enum class TimelineState { Travel, Service, Terminated, _ };

	private struct TimelineStyle {
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ line_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ footprint_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ cursor_color;

		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ icon_border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ icon_disabled_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ travel_icon_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ pause_icon_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ stop_icon_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ speed_icon_color;
	};

	/************************************************************************************************/
	class Timelinelet;

	private class ITimelineListener abstract {
	public:
		virtual void on_travel(WarGrey::SCADA::Timelinelet* master) {}
		virtual void on_step(WarGrey::SCADA::Timelinelet* master) {}
		virtual void on_service(WarGrey::SCADA::Timelinelet* master) {}
		virtual void on_terminate(WarGrey::SCADA::Timelinelet* master) {}

	public:
		virtual void on_startover(WarGrey::SCADA::Timelinelet* master, long long departure_ms, long long destination_ms) {}
		virtual void on_speed_shifted(WarGrey::SCADA::Timelinelet* master, unsigned int x) {}
		virtual void on_time_skipped(WarGrey::SCADA::Timelinelet* master, long long timepoint_ms) {}
	};

	private class Timelinelet
		: public virtual WarGrey::SCADA::IRangelet<long long>
		, public virtual WarGrey::SCADA::IStatelet<WarGrey::SCADA::TimelineState, WarGrey::SCADA::TimelineStyle> {
	public:
		~Timelinelet() noexcept;

		Timelinelet(long long tmin_ms, long long tmax_ms, float width, int frame_rate = 0, float thickness = 2.0F);
		Timelinelet(long long tmin_ms, long long tmax_ms, float width, unsigned int* speeds, size_t speeds_count,
			int frame_rate = 0, float thickness = 2.0F);

		template<size_t N>
		Timelinelet(long long tmin, long long tmax, float width, unsigned int (&speeds)[N], int frame_rate = 0, float thickness = 2.0F)
			: Timelinelet(tmin, tmax, width, speeds, N, frame_rate, thickness) {}

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void on_tap(float local_x, float local_y) override;
		bool can_change_range() override;

	public:
		void push_event_listener(WarGrey::SCADA::ITimelineListener* observer);
		long long get_departure_timepoint();
		long long get_destination_timepoint();
		unsigned int get_speed_shift();
		void shift_speed();
		void step(); // this is not designed for client applications

	protected:
		void prepare_style(WarGrey::SCADA::TimelineState state, WarGrey::SCADA::TimelineStyle& style) override;
		void apply_style(WarGrey::SCADA::TimelineStyle& style) override;
		void on_state_changed(WarGrey::SCADA::TimelineState state) override;
		void on_value_changed(long long timepoint_ms) override;
		void on_range_changed(long long departure_ms, long long destination_ms) override;

	private:
		void update_time_range();
		void update_speed_shift();

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ speedx;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ moment;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ timepoints;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ endpoint0;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ endpointn;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cursor;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ travel_icon;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pause_icon;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ stop_icon;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ speed_icon;

	private:
		float width;
		float height;
		float thickness;
		float footprint_thickness;
		float icon_radius;
		float endpoint_radius;
		float timeline_lx;
		float timeline_rx;

	private:
		std::deque<long long> footprints;
		unsigned int* speeds;
		size_t speeds_count;
		size_t speed_shift;

	private:
		std::deque<WarGrey::SCADA::ITimelineListener*> obsevers;
		WarGrey::SCADA::Timer^ timer;
		Platform::Object^ stepper;
		int frame_rate;
	};
}
