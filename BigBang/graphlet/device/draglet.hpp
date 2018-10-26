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

	float drag_depth(WarGrey::SCADA::DragInfo& info);

	/************************************************************************************************/
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
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	public:
		virtual Windows::Foundation::Numerics::float2 space_to_local(Windows::Foundation::Numerics::float3& X) = 0;

	public:
		void set_dredging(bool on) { this->dredging = on; }
		void set_position(float suction_depth,
			Windows::Foundation::Numerics::float3 ujoints[],
			Windows::Foundation::Numerics::float3& draghead,
			bool force = false);

	protected:
		virtual bool position_equal(Windows::Foundation::Numerics::float3& old_pos, Windows::Foundation::Numerics::float3& new_pos) = 0;
		virtual Platform::String^ position_label(Windows::Foundation::Numerics::float3& position) = 0;
		virtual void update_drag_head() = 0;
		
		virtual void on_position_changed(float suction_depth,
			Windows::Foundation::Numerics::float3 ujoints[],
			Windows::Foundation::Numerics::float3& draghead) {}

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ suction_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ dragarm_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hatchmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ draghead_part;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ visor_part;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ dragarm;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ universal_joint;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rubbers[DRAG_SEGMENT_MAX_COUNT];
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ meter_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color;

	protected:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ mfont;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ draghead_m;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ ujoints_ms[DRAG_SEGMENT_MAX_COUNT];
		unsigned int precision;
		
	protected:
		float width;
		float height;
		float thickness;
		float drag_thickness;
		float joint_radius;
		bool leftward;

	protected:
		float ws_x;
		float ws_y;
		float ws_width;
		float ws_height;

	protected:
		WarGrey::SCADA::DragInfo info;
		Windows::Foundation::Numerics::float3 suction;
		Windows::Foundation::Numerics::float3 ujoints[DRAG_SEGMENT_MAX_COUNT];
		Windows::Foundation::Numerics::float3 draghead;
		float drag_length;

	protected:
		Windows::Foundation::Numerics::float2 _suction;
		Windows::Foundation::Numerics::float2 _draghead;
		Windows::Foundation::Numerics::float2 _ujoints[DRAG_SEGMENT_MAX_COUNT];

	protected:
		double draghead_angle;
		float draghead_length;
		bool dredging;
	};

	private class DragXYlet : public WarGrey::SCADA::IDraglet {
	public:
		DragXYlet(WarGrey::SCADA::DragInfo& info, float width, float height, unsigned int color, float hatchmark_interval = 4.0F
			, unsigned int outside_step = 2U, unsigned int inside_step = 1U, float thickness = 2.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ meter_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color = nullptr,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		Windows::Foundation::Numerics::float2 space_to_local(Windows::Foundation::Numerics::float3& position) override;

	protected:
		bool position_equal(Windows::Foundation::Numerics::float3& old_pos, Windows::Foundation::Numerics::float3& new_pos) override;
		Platform::String^ position_label(Windows::Foundation::Numerics::float3& position) override;
		void update_drag_head() override;

	private:
		void draw_meter(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ meter, float joint_x, float joint_y, float gx);

	private:
		double outside_most;
		double inside_most;
		unsigned int step;
	};

	private class DragXZlet : public WarGrey::SCADA::IDraglet {
	public:
		DragXZlet(WarGrey::SCADA::DragInfo& info, float width, float height, unsigned int color,
			float hatchmark_interval = 5.0F, float suction_lowest = -20.0F, float thickness = 2.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ meter_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color = nullptr,
			Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = nullptr);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		Windows::Foundation::Numerics::float2 space_to_local(Windows::Foundation::Numerics::float3& position) override;

	protected:
		bool position_equal(Windows::Foundation::Numerics::float3& old_pos, Windows::Foundation::Numerics::float3& new_pos) override;
		Platform::String^ position_label(Windows::Foundation::Numerics::float3& position) override;
		void update_drag_head() override;
		void on_position_changed(float suction_depth,
			Windows::Foundation::Numerics::float3 ujoints[],
			Windows::Foundation::Numerics::float3& draghead) override;

	private:	
		void draw_meter(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ meter, float joint_x, float joint_y, float lX, float rX, float Y);

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ suction_m;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ head_joint;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ head_joint_border;

	private:
		float left_margin;
		float right_margin;

	private:
		double depth_highest;
		double depth_lowest;
		double suction_lowest;
	};

	/************************************************************************************************/
	private class DragHeadlet abstract : public WarGrey::SCADA::IGraphlet {
	public:
		DragHeadlet(float radius, unsigned int color,
			float depth_range = 60.0F, double visor_range = 60.0, double arm_range = 60.0,
			float thickness = 2.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ angle_pointer_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ suction_depth_pointer_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ draghead_depth_pointer_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* ts = nullptr, float* rs = nullptr, float* bs = nullptr, float* ls = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_position(float suction_depth,
			Windows::Foundation::Numerics::float3 ujoints[],
			Windows::Foundation::Numerics::float3& draghead,
			bool force = false);

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ depth_font;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ pointer_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hatchmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ draghead;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ visor;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ arm_pointer;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ visor_pointer;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ depth_pointer;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ visor_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ angle_pointer_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ suction_pointer_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ draghead_pointer_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color;

	private:
		float depth_interval;
		float depth_range;
		double visor_range;
		double arm_range;
		double offset;
		float radius;
		float thickness;
		float sign;
		float hspace;

	private:
		float visor_radius;
		float bottom_radius;
		float translate_x;
		float translate_y;
		float visor_pointer_radius;
		float arm_pointer_radius;
		float arrow_radius;
		float depth_top;
		float depth_height;

	private:
		double visor_pointer_degrees;
		double arm_pointer_degrees;
		float suction_depth;
		float draghead_depth;
	};
}
