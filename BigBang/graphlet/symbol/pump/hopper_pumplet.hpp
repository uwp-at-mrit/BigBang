#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class HopperPumpStatus {
		Running, Starting, Unstartable, Stopped, Stopping, Unstoppable, Ready, _
	};

	private struct HopperPumpStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ remote_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
	};

	private class HopperPumplet
		: public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::HopperPumpStatus, WarGrey::SCADA::HopperPumpStyle> {
	public:
		HopperPumplet(WarGrey::SCADA::HopperPumpStatus default_status, float radiusX, float radiusY = 0.0F, double degrees = 0.0);
		HopperPumplet(float radiusX, float radiusY = 0.0F, double degrees = 0.0);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_remote_control(bool on);
		void fill_pump_origin(float* x = nullptr, float* y = nullptr);

	protected:
		void prepare_style(WarGrey::SCADA::HopperPumpStatus status, WarGrey::SCADA::HopperPumpStyle& style) override;
		void on_status_changed(HopperPumpStatus status) override;
		
	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ unstartable_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ unstoppable_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ border;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ inlet;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ iborder;
		
	private:
		Windows::Foundation::Rect enclosing_box;
		float iradius;
		float pump_cx;
		float pump_cy;
		bool leftward;
		bool upward;

	private:
		double mask_percentage;
		float mask_cx;
		float mask_cy;
		bool remote_control;
	};
}
