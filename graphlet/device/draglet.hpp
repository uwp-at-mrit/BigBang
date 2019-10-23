#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
#define DRAG_SEGMENT_MAX_COUNT 3

	private struct DragInfo {
		double trunnion_gapsize;
		double trunnion_length;
		double pipe_lengths[DRAG_SEGMENT_MAX_COUNT];
		double pipe_padding;
		double pipe_radius;
		double head_width;
		double head_length;
		double head_height;
		double head_compensation;
		double visor_degrees_min;
		double visor_degrees_max;
		double arm_degrees_min;
		double arm_degrees_max;
	};

	private struct DragStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_meter_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ joint_meter_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ arm_angle_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ joint_angle_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color;
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font;

		unsigned int precision;
		float thickness;
	};

	private struct DragLinesStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ tide_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ target_depth_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ tolerance_depth_color;

		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ stroke;

		float thickness;
	};

	double drag_length(WarGrey::SCADA::DragInfo& info);
	double drag_depth(WarGrey::SCADA::DragInfo& info, double max_depth_degrees = 60.0);

	WarGrey::SCADA::DragStyle drag_default_style(unsigned int color,
		unsigned int precision = 2U, float fontsize = 24.0F, float thickness = 2.0F);

	WarGrey::SCADA::DragLinesStyle drag_default_lines_style(
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ stroke
		= WarGrey::SCADA::make_dash_stroke(Microsoft::Graphics::Canvas::Geometry::CanvasDashStyle::Dash),
		float thickness = 2.0F);

	/************************************************************************************************/
	private class IDraglet abstract : public WarGrey::SCADA::IGraphlet {
	public:
		IDraglet(WarGrey::SCADA::DragInfo& info, WarGrey::SCADA::DragStyle& style, bool leftward);

	public:
		virtual Windows::Foundation::Numerics::float2 space_to_local(WarGrey::SCADA::double3& X) = 0;

	public:
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	public:
		void set_dredging(bool on) { this->dredging = on; }
		void set_figure(WarGrey::SCADA::double3& trunnion, WarGrey::SCADA::double3 ujoints[], WarGrey::SCADA::double3& draghead, double visor_angle, bool force = false);

	public:
		double get_arm_degrees(unsigned int idx = DRAG_SEGMENT_MAX_COUNT);
		double get_ujoint_degrees(unsigned int idx = DRAG_SEGMENT_MAX_COUNT);
		double get_visor_earth_degrees();

	protected:
		virtual double arctangent(WarGrey::SCADA::double3& pt1, WarGrey::SCADA::double3& pt2) = 0;

	protected:
		virtual bool position_equal(WarGrey::SCADA::double3& old_pos, WarGrey::SCADA::double3& new_pos) = 0;
		virtual Platform::String^ position_label(WarGrey::SCADA::double3& position) = 0;
		virtual Platform::String^ angle_label(double degrees);
		virtual void update_drag_head() = 0;
		
		virtual void on_position_changed(WarGrey::SCADA::double3& trunnion, WarGrey::SCADA::double3 ujoints[], WarGrey::SCADA::double3& draghead) {}

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
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ forejoint_deg;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ ujoint_ms[DRAG_SEGMENT_MAX_COUNT];
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ arm_degs[DRAG_SEGMENT_MAX_COUNT];
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ joint_degs[DRAG_SEGMENT_MAX_COUNT];
		
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
		WarGrey::SCADA::double3 suction;
		WarGrey::SCADA::double3 trunnion;
		WarGrey::SCADA::double3 ujoints[DRAG_SEGMENT_MAX_COUNT];
		WarGrey::SCADA::double3 draghead;
		double arm_angles[DRAG_SEGMENT_MAX_COUNT];
		double joint_angles[DRAG_SEGMENT_MAX_COUNT];
		double forearm_angle;
		double forejoint_angle;
		double drag_length;

	protected: // graphlets 2D coordinates
		Windows::Foundation::Numerics::float2 _suction;
		Windows::Foundation::Numerics::float2 _trunnion;
		Windows::Foundation::Numerics::float2 _draghead;
		Windows::Foundation::Numerics::float2 _ujoints[DRAG_SEGMENT_MAX_COUNT];
		double _forearm_angle;

	protected:
		double visor_angle;
		float draghead_length;
		float visor_length;
		bool dredging;
	};

	private class DragXYlet : public WarGrey::SCADA::IDraglet {
	public:
		DragXYlet(WarGrey::SCADA::DragInfo& info, WarGrey::SCADA::DragStyle& style,
			float ws_height, float hatchmark_interval = 4.0F, unsigned int outboard_step = 3U, unsigned int inboard_step = 2U);

	public:
		Windows::Foundation::Numerics::float2 space_to_local(WarGrey::SCADA::double3& position) override;

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		double arctangent(WarGrey::SCADA::double3& pt1, WarGrey::SCADA::double3& pt2) override;
		
	protected:
		bool position_equal(WarGrey::SCADA::double3& old_pos, WarGrey::SCADA::double3& new_pos) override;
		Platform::String^ position_label(WarGrey::SCADA::double3& position) override;
		void update_drag_head() override;

	private:
		void draw_metrics(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ meter, float joint_x, float joint_y, float gx, float hspace,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

	private:
		double outboard_most;
		double inboard_most;
		unsigned int step;
	};

	private class DragYZlet : public WarGrey::SCADA::IDraglet {
	public:
		DragYZlet(WarGrey::SCADA::DragInfo& info, WarGrey::SCADA::DragStyle& style, WarGrey::SCADA::DragLinesStyle& line_style,
			float ws_width, double max_depth_degrees = 45.0, float vhatchmark_interval = 5.0F, float hhatchmark_interval = 4.0F,
			unsigned int outboard_step = 3U, unsigned int inboard_step = 2U);

	public:
		Windows::Foundation::Numerics::float2 space_to_local(WarGrey::SCADA::double3& position) override;

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_tide_mark(double tidemark, bool force = false);
		void set_design_depth(double target, double tolerance, bool force = false);

	protected:
		double arctangent(WarGrey::SCADA::double3& pt1, WarGrey::SCADA::double3& pt2) override;

	protected:
		bool position_equal(WarGrey::SCADA::double3& old_pos, WarGrey::SCADA::double3& new_pos) override;
		Platform::String^ position_label(WarGrey::SCADA::double3& position) override;
		void update_drag_head() override;

		void on_position_changed(WarGrey::SCADA::double3& trunnion, WarGrey::SCADA::double3 ujoints[], WarGrey::SCADA::double3& draghead) override;

	private:
		float y_to_x(double y);
		float z_to_y(double z);

	private:
		void draw_metrics(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ meter, float joint_x, float joint_y,
			float lX, float rX, float tY, float bY, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color, bool below);

		void draw_line(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x0, float xn, float y,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ suction_m;
		WarGrey::SCADA::DragLinesStyle lines_style;

	private:
		double depth_highest;
		double depth_lowest;
		double outboard_most;
		double inboard_most;
		unsigned int hstep;

	private:
		float tidemark;
		float silt_depth;
		float target_depth;
		float tolerance_depth;
	};

	private class DragXZlet : public WarGrey::SCADA::IDraglet {
	public:
		DragXZlet(WarGrey::SCADA::DragInfo& info, WarGrey::SCADA::DragStyle& style, WarGrey::SCADA::DragLinesStyle& line_style,
			float ws_width, double max_depth_degrees = 45.0,
			float hatchmark_interval = 5.0F, float suction_lowest = -15.0F);

	public:
		Windows::Foundation::Numerics::float2 space_to_local(WarGrey::SCADA::double3& position) override;

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_tide_mark(double tidemark, bool force = false);
		void set_design_depth(double target, double tolerance, bool force = false);

	protected:
		double arctangent(WarGrey::SCADA::double3& pt1, WarGrey::SCADA::double3& pt2) override;

	protected:
		bool position_equal(WarGrey::SCADA::double3& old_pos, WarGrey::SCADA::double3& new_pos) override;
		Platform::String^ position_label(WarGrey::SCADA::double3& position) override;
		void update_drag_head() override;

		void on_position_changed(WarGrey::SCADA::double3& trunnion, WarGrey::SCADA::double3 ujoints[], WarGrey::SCADA::double3& draghead) override;

	private:
		float x_to_x(double x);
		float z_to_y(double z);

	private:
		void draw_metrics(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
			Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ meter, float joint_x, float joint_y,
			float lX, float rX, float tY, float bY, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color, bool below);

		void draw_line(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x0, float xn, float y,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ suction_m;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ joint_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ x_axis;
		WarGrey::SCADA::DragLinesStyle lines_style;

	private:
		float left_margin;
		float right_margin;

	private:
		double depth_highest;
		double depth_lowest;
		double suction_lowest;

	private:
		float tidemark;
		float silt_depth;
		float target_depth;
		float tolerance_depth;

	private:
		float mask_dx;
		float mask_dy;
	};

	/************************************************************************************************/
	private class DragHeadlet abstract : public WarGrey::SCADA::IGraphlet {
	public:
		DragHeadlet(WarGrey::SCADA::DragInfo& info, float radius, unsigned int color,
			double max_depth_degrees = 45.0, float thickness = 2.0F,
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
		void set_depths(double suction_depth, double draghead_depth, bool force = false);

	public:
		void show_depth_metrics(bool yes_or_no);
		
	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ pointer_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ angle_hatchmark;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ depth_hatchmark;
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
		
	private:
		WarGrey::SCADA::DragInfo info;
		double depth_interval;
		double depth_range;
		double offset;
		float radius;
		float thickness;
		float sign;
		float tspace;
		float arm_bottom;
		float teeth_y;

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

	private:
		bool depth_shown;
		double suction_depth;
		double draghead_depth;
		float depth_x;
	};
}
