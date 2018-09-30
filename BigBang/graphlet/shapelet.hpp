#pragma once

#include <list>

#include "graphlet/primitive.hpp"

#include "credit.hpp"
#include "turtle.hpp"
#include "brushes.hxx"
#include "geometry.hpp"

namespace WarGrey::SCADA {
	private class Shapelet : public virtual WarGrey::SCADA::IGraphlet {
	public:
		Shapelet(Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ shape,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_color(unsigned int color);
		void set_color(Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color);

		void set_border_color(unsigned int color);
		void set_border_color(Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color);
		
		void fill_shape_origin(float* x, float* y);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ surface;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ border;
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

	private:
		Windows::Foundation::Rect border_box;
		Windows::Foundation::Rect box;
	};

	private class Shiplet : public WarGrey::SCADA::Shapelet {
	public:
		Shiplet(float length, float radius, unsigned int border_color, float thickness = 1.0F);

		Shiplet(float length, float radius,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F);
	};

	private class Rectanglet : public WarGrey::SCADA::Shapelet {
	public:
		Rectanglet(float edge_size, unsigned int border_color, float thickness = 1.0F);
		Rectanglet(float width, float height, unsigned int border_color, float thickness = 1.0F);

		Rectanglet(float edge_size,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F);

		Rectanglet(float width, float height,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F);
	};

	private class Circlelet : public WarGrey::SCADA::Shapelet {
	public:
		Circlelet(float radius, unsigned int border_color, float thickness = 1.0F);

		Circlelet(float radius,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F);
	};

	private class Sectorlet : public WarGrey::SCADA::Shapelet {
	public:
		Sectorlet(double sdegrees, double edegrees, float radiusX, float radiusY, unsigned int border_color, float thickness = 1.0F);

		Sectorlet(double sdegrees, double edegrees, float radiusX, float radiusY,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F);
	};

	private class Segmentlet : public WarGrey::SCADA::Shapelet {
	public:
		Segmentlet(double sdegrees, double edegrees, float radiusX, float radiusY, unsigned int border_color, float thickness = 1.0F);

		Segmentlet(double sdegrees, double edegrees, float radiusX, float radiusY,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F);
	};

	private class ArrowHeadlet : public WarGrey::SCADA::Shapelet {
	public:
		ArrowHeadlet(float radius, double degrees, unsigned int border_color, float thickness = 1.0F);

		ArrowHeadlet(float radius, double degrees,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ border_color = WarGrey::SCADA::Colours::Transparent,
			float thickness = 1.0F);

	public:
		double get_degrees();

	private:
		double start_degrees;
	};

	private class Linelet : public virtual WarGrey::SCADA::Shapelet {
	public:
		Linelet(float sx, float sy, float ex, float ey, float thickness, unsigned int color,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

		Linelet(float sx, float sy, float ex, float ey, float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = WarGrey::SCADA::Colours::Silver,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);
	};

	private class Arclet : public virtual WarGrey::SCADA::Shapelet {
	public:
		Arclet(double sdegrees, double edegrees, float radiusX, float radiusY, float thickness, unsigned int color,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

		Arclet(double sdegrees, double edegrees, float radiusX, float radiusY, float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = WarGrey::SCADA::Colours::Silver,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);
	};

	private class Omegalet : public virtual WarGrey::SCADA::Shapelet {
	public:
		Omegalet(double sdegrees, float radius, float thickness, unsigned int color,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

		Omegalet(double sdegrees, float radius, float thickness = 1.0F,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = WarGrey::SCADA::Colours::Silver,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);
	};

	template<typename Anchor>
	private class Tracklet : public virtual WarGrey::SCADA::Shapelet {
	public:
		~Tracklet() noexcept {
			this->turtle->destroy();
		}

		Tracklet(WarGrey::SCADA::Turtle<Anchor>* turtle, float thickness, unsigned int color,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr)
			: Tracklet<Anchor>(turtle, thickness, WarGrey::SCADA::Colours::make(color), style) {}

		Tracklet(WarGrey::SCADA::Turtle<Anchor>* turtle, float thickness = 1.0F
			, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color = WarGrey::SCADA::Colours::Silver
			, Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr)
			: Shapelet(turtle->snap_track(thickness, style), color), turtle(turtle)
			, thickness(thickness), style(style) {
			this->turtle->reference();
		}

	public:
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
			auto subtrack = this->subtracks.begin();
			auto subcolor = this->subcolors.begin();

			WarGrey::SCADA::Shapelet::draw(ds, x, y, Width, Height);

			while (subtrack != this->subtracks.end()) {
				ds->DrawCachedGeometry((*subtrack), x, y, (*subcolor));

				subtrack++;
				subcolor++;
			}
		}

	public:
		void fill_anchor_location(Anchor a, float* x, float* y, bool need_absolute_location = true) {
			float raw_x, raw_y, shape_x, shape_y;
			float x0 = 0.0F;
			float y0 = 0.0F;

			this->fill_shape_origin(&shape_x, &shape_y);
			this->turtle->fill_anchor_location(a, &raw_x, &raw_y);

			if (need_absolute_location && (this->info != nullptr)) {
				this->info->master->fill_graphlet_location(this, &x0, &y0);
			}

			SET_BOX(x, raw_x + x0 - shape_x);
			SET_BOX(y, raw_y + y0 - shape_y);
		}

		void append_subtrack(Anchor a1, Anchor a2, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color) {
			this->append_subtrack(this->turtle->subtrack(a1, a2, this->thickness, this->style), color);
		}

		void append_subtrack(Anchor as[], unsigned int count, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color) {
			this->append_subtrack(this->turtle->subtrack(as, count, this->thickness, this->style), color);
		}

		template<size_t N>
		void append_subtrack(Anchor (&as)[N], Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color) {
			return this->append_subtrack(as, N, color);
		}

		void clear_subtacks() {
			this->subtracks.clear();
			this->subcolors.clear();
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
		void append_subtrack(Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ track
			, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color) {
			float shape_x, shape_y;

			this->fill_shape_origin(&shape_x, &shape_y);
			this->subtracks.push_back(geometry_freeze(geometry_translate(track, -shape_x, -shape_y)));
			this->subcolors.push_back(color);
		}

	private:
		WarGrey::SCADA::Turtle<Anchor>* turtle;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style;
		float thickness;

	private:
		std::list<Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^> subtracks;
		std::list<Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^> subcolors;
	};

	private class HLinelet : public WarGrey::SCADA::IGraphlet {
	public:
		HLinelet(float thickness, unsigned int color,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

		HLinelet(float thickness,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	public:
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style;
		float thickness;
	};

	private class VLinelet : public WarGrey::SCADA::IGraphlet {
	public:
		VLinelet(float thickness, unsigned int color,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

		VLinelet(float thickness,
			Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color,
			Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style = nullptr);

	public:
		void fill_extent(float x, float y, float* width, float* height) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ style;
		float thickness;
	};
}
