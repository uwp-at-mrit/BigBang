#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class WaterPumpStatus {
		Running, Unstartable, Stopped, Unstoppable, _
	};

	private struct WaterPumpStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
	};

	private class WaterPumplet
		: public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::WaterPumpStatus, WarGrey::SCADA::WaterPumpStyle> {

	public:
		WaterPumplet(WarGrey::SCADA::WaterPumpStatus default_status, float radius, double degrees = 0.0);
		WaterPumplet(float radius, double degrees = 0.0);

	public:
		void construct() override;
		void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void fill_pump_origin(float* x = nullptr, float* y = nullptr);

	protected:
		void prepare_style(WarGrey::SCADA::WaterPumpStatus status, WarGrey::SCADA::WaterPumpStyle& style) override;
		
	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ border;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ indicator;
		
	private:
		Windows::Foundation::Rect enclosing_box;
		float pump_cx;
		float pump_cy;
		bool leftward;
	};
}
