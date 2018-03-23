#pragma once

#include "snip/snip.hpp"
#include "turtle.idl"

namespace WarGrey::SCADA {
	private class Shapelet : public WarGrey::SCADA::ISnip {};

	private class Geometrylet : public WarGrey::SCADA::Shapelet {
	public:
		Geometrylet(Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ shape, Windows::UI::Color& color,
			Windows::UI::Color& border_color = Windows::UI::Colors::Transparent,
			float thickness = 1.0F);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ surface;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

	protected:
		Windows::Foundation::Rect box;
	};

	template<typename Anchor>
	private class Tracklet : public WarGrey::SCADA::Geometrylet {
	public:
		~Tracklet() noexcept {
			this->turtle->destroy();
		}

		Tracklet(WarGrey::SCADA::Turtle<Anchor>* turtle, float thickness = 1.0F
			, Windows::UI::Color& color = Windows::UI::Colors::Silver)
			: Geometrylet(turtle->snap_track(thickness), color), turtle(turtle) {
			this->turtle->reference();
		}

	public:
		void fill_anchor_location(Anchor node, float* x, float* y) {
			float raw_x, raw_y;

			this->turtle->fill_anchor_location(node, &raw_x, &raw_y);

			SET_BOX(x, raw_x - WarGrey::SCADA::Geometrylet::box.X);
			SET_BOX(y, raw_y - WarGrey::SCADA::Geometrylet::box.Y);
		}

	private:
		WarGrey::SCADA::Turtle<Anchor>* turtle;
		float thickness;
	};
}
