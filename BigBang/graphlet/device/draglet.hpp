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
		double visor_degrees_min;
		double visor_degrees_max;
		double arm_degrees_min;
		double arm_degrees_max;
	};

	private struct DragStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ meter_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ angle_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

		unsigned int precision;
		float thickness;
	};

	float drag_depth(WarGrey::SCADA::DragInfo& info);

	WarGrey::SCADA::DragStyle drag_default_style(unsigned int color,
		unsigned int precision = 2U, float fontsize = 24.0F, float thickness = 2.0F);

	/************************************************************************************************/
	private class IDraglet abstract : public WarGrey::SCADA::IGraphlet {
	public:
		IDraglet(WarGrey::SCADA::DragInfo& info, WarGrey::SCADA::DragStyle& style, float width, float height);

	public:
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	public:
		void set_dredging(bool on) { this->dredging = on; }
		void set_figure(Windows::Foundation::Numerics::float3& trunnion,
			Windows::Foundation::Numerics::float3 ujoints[],
			Windows::Foundation::Numerics::float3& draghead,
			double visor_angle,
			bool force = false);

	public:
		double get_arm_degrees(unsigned int idx = DRAG_SEGMENT_MAX_COUNT);
		double get_visor_earth_degrees();

	protected:
		virtual Windows::Foundation::Numerics::float2 space_to_local(Windows::Foundation::Numerics::float3& X) = 0;
		virtual double arctangent(Windows::Foundation::Numerics::float3& pt1, Windows::Foundation::Numerics::float3& pt2) = 0;

	protected:
		virtual bool position_equal(Windows::Foundation::Numerics::float3& old_pos, Windows::Foundation::Numerics::float3& new_pos) = 0;
		virtual Platform::String^ position_label(Windows::Foundation::Numerics::float3& position) = 0;
		virtual void update_drag_head() = 0;
		
		virtual void on_position_changed(Windows::Foundation::Numerics::float3& trunnion,
			Windows::Foundation::Numerics::float3 ujoints[],
			Windows::Foundation::Numerics::float3& draghead) {}

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hatchmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ draghead_part;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ visor_part;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ drag_body;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ universal_joint;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rubbers[DRAG_SEGMENT_MAX_COUNT];

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ suction_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ dragarm_style;
		WarGrey::SCADA::DragStyle& style;

	protected:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ draghead_m;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ forearm_deg;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ ujoints_ms[DRAG_SEGMENT_MAX_COUNT];
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ arm_degs[DRAG_SEGMENT_MAX_COUNT];
		
	protected:
		float width;
		float height;
		float drag_thickness;
		float joint_radius;
		bool leftward;

	protected:
		float ws_x;
		float ws_y;
		float ws_width;
		float ws_height;

	protected: // real 3D coordinates
		WarGrey::SCADA::DragInfo info;
		Windows::Foundation::Numerics::float3 pseudo_suction; // real suction does not affects the form of drags.
		Windows::Foundation::Numerics::float3 trunnion;
		Windows::Foundation::Numerics::float3 ujoints[DRAG_SEGMENT_MAX_COUNT];
		Windows::Foundation::Numerics::float3 draghead;
		double arm_angles[DRAG_SEGMENT_MAX_COUNT];
		double forearm_angle;
		float drag_length;

	protected: // graphlets 2D coordinates
		Windows::Foundation::Numerics::float2 _pseudo_suction;
		Windows::Foundation::Numerics::float2 _trunnion;
		Windows::Foundation::Numerics::float2 _draghead;
		Windows::Foundation::Numerics::float2 _ujoints[DRAG_SEGMENT_MAX_COUNT];
		double _forearm_angle;

	protected:
		double visor_angle;
		float draghead_length;
		bool dredging;
	};

	private class DragXYlet : public WarGrey::SCADA::IDraglet {
	public:
		DragXYlet(WarGrey::SCADA::DragInfo& info, WarGrey::SCADA::DragStyle& style,
			float width, float height, float hatchmark_interval = 5.0F,
			unsigned int outside_step = 2U, unsigned int inside_step = 1U);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		Windows::Foundation::Numerics::float2 space_to_local(Windows::Foundation::Numerics::float3& position) override;
		double arctangent(Windows::Foundation::Numerics::float3& pt1, Windows::Foundation::Numerics::float3& pt2) override;
		
	protected:
		bool position_equal(Windows::Foundation::Numerics::float3& old_pos, Windows::Foundation::Numerics::float3& new_pos) override;
		Platform::String^ position_label(Windows::Foundation::Numerics::float3& position) override;
		void update_drag_head() override;

	private:
		void draw_metrics(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ meter, float joint_x, float joint_y, float gx, float hspace,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

	private:
		double outside_most;
		double inside_most;
		unsigned int step;
	};

	private class DragXZlet : public WarGrey::SCADA::IDraglet {
	public:
		DragXZlet(WarGrey::SCADA::DragInfo& info, WarGrey::SCADA::DragStyle& style,
			float width, float height, float hatchmark_interval = 5.0F, float suction_lowest = -20.0F);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		Windows::Foundation::Numerics::float2 space_to_local(Windows::Foundation::Numerics::float3& position) override;
		double arctangent(Windows::Foundation::Numerics::float3& pt1, Windows::Foundation::Numerics::float3& pt2) override;

	protected:
		bool position_equal(Windows::Foundation::Numerics::float3& old_pos, Windows::Foundation::Numerics::float3& new_pos) override;
		Platform::String^ position_label(Windows::Foundation::Numerics::float3& position) override;
		void update_drag_head() override;

	private:	
		void draw_metrics(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ meter, float joint_x, float joint_y, float lX, float rX, float Y,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ suction_m;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ joint_mask;

	private:
		float left_margin;
		float right_margin;

	private:
		double depth_highest;
		double depth_lowest;
		double suction_lowest;

	private:
		float mask_dx;
		float mask_dy;
	};

	/************************************************************************************************/
	private class DragHeadlet abstract : public WarGrey::SCADA::IGraphlet {
	public:
		DragHeadlet(WarGrey::SCADA::DragInfo& info, float radius, unsigned int color, float thickness = 2.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ angle_pointer_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ suction_depth_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ draghead_depth_pointer_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* ts = nullptr, float* rs = nullptr, float* bs = nullptr, float* ls = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_angles(double visor_degrees, double arm_degrees, bool force = false);
		void set_depths(float suction_depth, float draghead_depth, bool force = false);
		
	private:
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
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ depth_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ suction_m;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ depth_m;
		unsigned int precision;
		float depth_x;

	private:
		WarGrey::SCADA::DragInfo info;
		float depth_interval;
		float depth_range;
		double offset;
		float radius;
		float thickness;
		float sign;
		float vspace;

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
		double arm_earth_degrees;
		double visor_degrees;
		float suction_depth;
		float draghead_depth;
	};
}
