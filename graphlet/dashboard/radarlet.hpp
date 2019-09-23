#pragma once

#include "graphlet/primitive.hpp"

#include "datum/time.hpp"
#include "datum/object.hpp"

#include "tongue.hpp"
#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private struct RadarStyle {
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
	private class IRadarlet abstract : public WarGrey::SCADA::IGraphlet {
	public:
		IRadarlet(float radius, unsigned int count);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ vaxes;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ hmarks;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ haxes;

	private:
		WarGrey::SCADA::RadarStyle style;

	private:
		unsigned int count;
		float radius;
	};

	template<typename E>
	private class Radarlet : public WarGrey::SCADA::IRadarlet {
	public:
		Radarlet(float radius) : IRadarlet(radius, _N(E)) {}
	};
}
