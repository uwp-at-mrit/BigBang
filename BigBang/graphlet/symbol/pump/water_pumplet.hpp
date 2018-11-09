#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class WaterPumpStatus {
		Running, StartReady, Starting, Unstartable, Stopped, StopReady, Stopping, Unstoppable, Ready, Broken, _
	};

	private struct WaterPumpStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ remote_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
	};

	private class WaterPumplet
		: public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::WaterPumpStatus, WarGrey::SCADA::WaterPumpStyle> {

	public:
		WaterPumplet(WarGrey::SCADA::WaterPumpStatus default_status, float radius, double degrees = 0.0);
		WaterPumplet(float radius, double degrees = 0.0);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_remote_control(bool on);
		void fill_pump_origin(float* x = nullptr, float* y = nullptr);

	protected:
		void prepare_style(WarGrey::SCADA::WaterPumpStatus status, WarGrey::SCADA::WaterPumpStyle& style) override;
		void on_status_changed(WaterPumpStatus status) override;
		
	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ start_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ stop_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ border;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		
	private:
		Windows::Foundation::Rect enclosing_box;
		float iradius;
		float pump_cx;
		float pump_cy;
		bool leftward;

	private:
		bool remote_control;
	};
}
