#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private value struct DragInfo {
		float trunnion_gapsize;
		float trunnion_length;
		float pipe_lengths[3];
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
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;

	public:
		void set_position(
			Windows::Foundation::Numerics::float3& trunnion,
			Windows::Foundation::Numerics::float3 intermediates[],
			Windows::Foundation::Numerics::float3& draghead,
			bool force = false);

	protected:
		virtual bool position_equal(Windows::Foundation::Numerics::float3& old_pos, Windows::Foundation::Numerics::float3& new_pos) = 0;
		virtual void on_position_changed(
			Windows::Foundation::Numerics::float3& trunnion,
			Windows::Foundation::Numerics::float3 intermediates[],
			Windows::Foundation::Numerics::float3& draghead) = 0;

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ suction_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hatchmarks;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color;
		
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

	protected:
		WarGrey::SCADA::DragInfo info;
		Windows::Foundation::Numerics::float3 trunnion;
		Windows::Foundation::Numerics::float3 intermediates[3];
		Windows::Foundation::Numerics::float3 draghead;
		float length;
	};

	private class DragXZlet : public WarGrey::SCADA::IDraglet {
	public:
		DragXZlet(WarGrey::SCADA::DragInfo& info, float width, float height, unsigned int color,
			float thickness = 2.0F, float hatchmark_interval = 5.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color = nullptr);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		bool position_equal(Windows::Foundation::Numerics::float3& old_pos, Windows::Foundation::Numerics::float3& new_pos) override;
		void on_position_changed(
			Windows::Foundation::Numerics::float3& trunnion,
			Windows::Foundation::Numerics::float3 intermediates[],
			Windows::Foundation::Numerics::float3& draghead) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pivot;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ value_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ density_value;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ flspeed_value;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ density_unit;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ flspeed_unit;
		float label_rx;
		float label_by;

	private:
		double depth_highest;
		double depth_lowest;
		double suction_lowest;

	private:
		float draghead_pinx;
		float draghead_piny;
		float trunnion_pinx;
		float trunnion_piny;
		float intermediate_pinxs[3];
		float intermediate_pinys[3];
	};
}
