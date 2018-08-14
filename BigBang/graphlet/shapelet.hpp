#pragma once

#include "graphlet/primitive.hpp"
#include "credit.hpp"
#include "turtle.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class IShapelet : public virtual WarGrey::SCADA::IGraphlet {
	public:
		IShapelet(Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ shape,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ surface;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ border;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color;

	protected:
		Windows::Foundation::Rect box;
	};

	private class Rectanglelet : public WarGrey::SCADA::IShapelet {
	public:
		Rectanglelet(float edge_size,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F);

		Rectanglelet(float width, float height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F);
	};

	template<typename Anchor>
	private class Tracklet : public virtual WarGrey::SCADA::IShapelet {
	public:
		~Tracklet() noexcept {
			this->turtle->destroy();
		}

		Tracklet(WarGrey::SCADA::Turtle<Anchor>* turtle, float thickness = 1.0F
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = WarGrey::SCADA::Colours::Silver)
			: IShapelet(turtle->snap_track(thickness), color), turtle(turtle) {
			this->turtle->reference();
		}

	public:
		void fill_anchor_location(Anchor a, float* x, float* y, bool need_absolute_location = false) {
			float raw_x, raw_y;
			float x0 = 0.0F;
			float y0 = 0.0F;

			this->turtle->fill_anchor_location(a, &raw_x, &raw_y);

			if (need_absolute_location && (this->info != nullptr)) {
				this->info->master->fill_graphlet_location(this, &x0, &y0);
			}

			SET_BOX(x, raw_x + x0 - this->box.X);
			SET_BOX(y, raw_y + y0 - this->box.Y);
		}

	public:
		template<class G>
		void map_credit_graphlet(WarGrey::SCADA::Credit<G, Anchor>* g, GraphletAnchor a = GraphletAnchor::CC, float dx = 0.0F, float dy = 0.0F) {
			if (g != nullptr) {
				this->map_graphlet_at_anchor(g, g->id, a, dx, dy);
			}
		}

		template<class G>
		void map_graphlet_at_anchor(G* g, Anchor a, GraphletAnchor ga = GraphletAnchor::CC, float dx = 0.0F, float dy = 0.0F) {
			if (this->info != nullptr) {
				float anchor_x, anchor_y;

				this->fill_anchor_location(a, &anchor_x, &anchor_y, true);
				this->info->master->move_to(g, anchor_x + dx, anchor_y + dy, ga);
			}
		}

	private:
		WarGrey::SCADA::Turtle<Anchor>* turtle;
	};
}
