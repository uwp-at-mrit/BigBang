#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class ITanklet abstract : public virtual WarGrey::SCADA::IGraphlet {
	public:
		ITanklet(float width, float height = 0.0F, float thickness = 3.0F);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void update_liquid_level(float percentage, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ liquid;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ floating;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ indicator;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ tube;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ ruler;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ liquid_color;

	private:
		float width;
		float height;
		float thickness;
		
	private:
		float liquid_y;
		float float_half_height;
		float ruler_em;
	};
}
