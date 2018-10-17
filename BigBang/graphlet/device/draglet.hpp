#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Draglet : public WarGrey::SCADA::IGraphlet {
	public:
		Draglet(double depth_highest, double depth_lowest, double trunion_lowest, float width, float height,
			unsigned int visor_color, float thickness = 2.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color = nullptr);

		Draglet(float width, float height, unsigned int visor_color, float thickness = 2.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color = nullptr,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_depths(double draghead, double intermediate, double trunnion, bool force = false);
		double get_trunnion_depth() { return this->trunnion_depth; }
		double get_intermediate_depth() { return this->intermediate_depth; }
		double get_draghead_depth() { return this->draghead_depth; }

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ suction_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hatchmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pivot;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ visor_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ head_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hatchmark_color;

	private:
		Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ value_font;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ density_value;
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ flspeed_value;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ density_unit;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ flspeed_unit;
		float label_rx;
		float label_by;

	private:
		float width;
		float height;
		float ws_top;
		float ws_height;
		float thickness;

	private:
		double depth_highest;
		double depth_lowest;
		double trunnion_lowest;
		bool leftward;

	private:
		float draghead_pinx;
		float draghead_piny;
		float draghead_length;
		float intermediate_pinx;
		float intermediate_piny;
		float intermediate_length;
		float trunnion_pinx;
		float trunnion_piny;
		float trunnion_length;
		float drag_thickness;

	private:
		double trunnion_depth;
		double intermediate_depth;
		double draghead_depth;
	};
}
