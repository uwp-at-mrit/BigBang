#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
#define DRAG_SEGMENT_MAX_COUNT 3

	private struct DragInfo {
		float trunnion_gapsize;
		float trunnion_length;
		float pipe_lengths[DRAG_SEGMENT_MAX_COUNT];
		float pipe_padding;
		float pipe_radius;
		float head_width;
		float head_length;
		float head_height;
		float head_compensation;
	};

	private class IDraglet abstract : public WarGrey::SCADA::IGraphlet {
	public:
		IDraglet(WarGrey::SCADA::DragInfo& info, float width, float height, float thickness,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ meter_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	public:
		void set_position(float suction_depth,
			Windows::Foundation::Numerics::float3 ujoints[],
			Windows::Foundation::Numerics::float3& draghead,
			bool force = false);

	protected:
		virtual bool position_equal(Windows::Foundation::Numerics::float3& old_pos, Windows::Foundation::Numerics::float3& new_pos) = 0;
		virtual void on_position_changed(float suction_depth,
			Windows::Foundation::Numerics::float3 ujoints[],
			Windows::Foundation::Numerics::float3& draghead) = 0;

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ suction_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hatchmarks;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ meter_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color;

	protected:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ mfont;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ suction_m;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ draghead_m;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ ujoints_ms[DRAG_SEGMENT_MAX_COUNT];
		unsigned int precision;
		
	protected:
		float width;
		float height;
		float thickness;
		float drag_thickness;
		bool leftward;

	protected:
		float ws_x;
		float ws_y;
		float ws_width;
		float ws_height;
		float ws_margin;

	protected:
		WarGrey::SCADA::DragInfo info;
		Windows::Foundation::Numerics::float3 ujoints[DRAG_SEGMENT_MAX_COUNT];
		Windows::Foundation::Numerics::float3 draghead;
		float suction_depth;
		float total_length;
	};

	private class DragXZlet : public WarGrey::SCADA::IDraglet {
	public:
		DragXZlet(WarGrey::SCADA::DragInfo& info, float width, float height, unsigned int color,
			float thickness = 2.0F, float hatchmark_interval = 5.0F, float suction_lowest = -20.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ meter_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color = nullptr,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		bool position_equal(Windows::Foundation::Numerics::float3& old_pos, Windows::Foundation::Numerics::float3& new_pos) override;
		void on_position_changed(float suction_depth,
			Windows::Foundation::Numerics::float3 ujoints[],
			Windows::Foundation::Numerics::float3& draghead) override;

	private:
		void draw_pipe_segment(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float ex, float ey, float sx, float sy);
		
		void draw_meter(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ meter, float joint_x, float joint_y, float lX, float rX, float Y);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ universal_joint;
		float left_margin;

	private:
		double depth_highest;
		double depth_lowest;
		double suction_lowest;

	private:
		float suction_y;
		float draghead_joint_x;
		float draghead_joint_y;
		float ujoints_xs[DRAG_SEGMENT_MAX_COUNT];
		float ujoints_ys[DRAG_SEGMENT_MAX_COUNT];
	};
}
