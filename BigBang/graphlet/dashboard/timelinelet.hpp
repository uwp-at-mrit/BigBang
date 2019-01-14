#pragma once
#pragma warning (disable: 4250)

#include "graphlet/primitive.hpp"

#include "paint.hpp"

namespace WarGrey::SCADA {
	private enum class TimelineState { Travel, Pause, Stop, _ };

	private struct TimelineStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ line_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ time0_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ footprint_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ timen_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ direction_color;

		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;
	};

	/************************************************************************************************/
	private class Timelinelet
		: public virtual WarGrey::SCADA::IRangelet<long long>
		, public virtual WarGrey::SCADA::IStatelet<WarGrey::SCADA::TimelineState, WarGrey::SCADA::TimelineStyle> {
	public:
		Timelinelet(long long tmin, long long tmax, float width, float thickness = 3.0F);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void on_tap(float local_x, float local_y) override;
		bool can_change_range() override;

	protected:
		void prepare_style(WarGrey::SCADA::TimelineState state, WarGrey::SCADA::TimelineStyle& style) override;
		void on_state_changed(WarGrey::SCADA::TimelineState state) override;
		void on_value_changed(long long timepoint) override;
		//void on_range_changed(long long time0, long long timen) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ vmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vaxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ haxes;

	private:


	private:
		float width;
		float height;
	};
}
