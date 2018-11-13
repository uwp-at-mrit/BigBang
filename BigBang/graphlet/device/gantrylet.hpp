#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class GantryStatus { Default, WindedUp, WindedOut, WindingUp, WindingOut, _ };

	private struct GantryStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ pulley_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ winding_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hat_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ base_color;
	};

	private struct GantrySymbolStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ highlight_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
	};

	private class Gantrylet : public WarGrey::SCADA::IStatuslet<WarGrey::SCADA::GantryStatus, WarGrey::SCADA::GantryStyle> {
	public:
		Gantrylet(float radius, float base_extent_ratio = 3.0F, double degrees = 42.0);
		Gantrylet(WarGrey::SCADA::GantryStatus default_status, float radius, float base_extent_ratio = 3.0F, double degrees = 42.0);

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
		void make_hat(double degress_ratio);

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ winding_style;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ hat;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bottom_base;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pivot_base;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ pulley;

	private:
		float pivot_base_radius;
		float bottom_base_radius;
		float base_extent_ratio;
		float pulley_radius;
		float pulley_cyoff;
		float hat_radiusX;
		float hat_radiusY;
		float hat_cxoff;
		float hat_cy;
		float hat_y;
		bool winding;

	private:
		double degrees;
		float radius;
		float width;
		float height;
		float thickness;

	private:
		bool leftward;
	};

	private class GantrySymbollet : public WarGrey::SCADA::IStatuslet<WarGrey::SCADA::GantryStatus, WarGrey::SCADA::GantrySymbolStyle> {
	public:
		GantrySymbollet(float width, float height = 0.0F);
		GantrySymbollet(WarGrey::SCADA::GantryStatus default_status, float width, float height = 0.0F);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::GantryStatus status, WarGrey::SCADA::GantrySymbolStyle& style) override;
		void on_status_changed(WarGrey::SCADA::GantryStatus status) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ leftward_arrow;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ rightward_arrow;

	private:
		float width;
		float height;
		float thickness;
	};
}
