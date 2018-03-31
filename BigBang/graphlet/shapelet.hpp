#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"
#include "turtle.idl"

namespace WarGrey::SCADA {
	private class Shapelet : public virtual WarGrey::SCADA::IGraphlet {};

	private class Geometrylet : public virtual WarGrey::SCADA::Shapelet {
	public:
		Geometrylet(Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ shape,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
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
	private class Tracklet : public virtual WarGrey::SCADA::Geometrylet {
	public:
		~Tracklet() noexcept {
			this->turtle->destroy();
		}

		Tracklet(WarGrey::SCADA::Turtle<Anchor>* turtle, float thickness = 1.0F
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = WarGrey::SCADA::Colours::Silver)
			: Geometrylet(turtle->snap_track(thickness), color), turtle(turtle) {
			this->turtle->reference();
		}

	public:
		void fill_anchor_location(Anchor node, float* x, float* y, bool need_absolute_location = false) {
			float raw_x, raw_y;
			float x0 = 0.0F;
			float y0 = 0.0F;

			this->turtle->fill_anchor_location(node, &raw_x, &raw_y);

			if (need_absolute_location && (this->info != nullptr)) {
				this->info->master->fill_graphlet_location(this, &x0, &y0);
			}

			SET_BOX(x, raw_x + x0 - WarGrey::SCADA::Geometrylet::box.X);
			SET_BOX(y, raw_y + y0 - WarGrey::SCADA::Geometrylet::box.Y);
		}

	private:
		WarGrey::SCADA::Turtle<Anchor>* turtle;
		float thickness;
	};
}
