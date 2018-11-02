#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class GantryStatus { WindedUp, WindedOut, WindingUp, WindingOut, _ };

	private struct GantryStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ pulley_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ winding_color;
	};

	private class Gantrylet : public WarGrey::SCADA::IStatuslet<WarGrey::SCADA::GantryStatus, WarGrey::SCADA::GantryStyle> {
	public:
		Gantrylet(float radius, float base_extent_ratio = 3.0F, double degrees = 30.0);
		Gantrylet(WarGrey::SCADA::GantryStatus default_status, float radius, float base_extent_ratio = 3.0F, double degrees = 30.0);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* t = nullptr, float* r = nullptr, float* b = nullptr, float* l = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		float get_winch_joint_y();

	protected:
		void prepare_style(WarGrey::SCADA::GantryStatus status, WarGrey::SCADA::GantryStyle& style) override;
		void on_status_changed(WarGrey::SCADA::GantryStatus status) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ winding_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ top;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ winded_top;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bottom_base;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pivot_base;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pulley;

	private:
		float pivot_base_radius;
		float bottom_base_radius;
		float base_extent_ratio;
		float pulley_radius;
		float top_radiusX;
		float top_radiusY;
		float winded_top_cy;
		float winded_top_y;
		bool winded_out;
		bool winding;

	private:
		double degrees;
		float width;
		float height;
		float thickness;

	private:
		bool leftward;
	};
}
