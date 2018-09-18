#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class CoolPumpStatus {
		Running, Unstartable, Stopped, Unstoppable, _
	};

	private struct CoolPumpStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
	};

	private class CoolPumplet
		: public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::CoolPumpStatus, WarGrey::SCADA::CoolPumpStyle> {
	public:
		CoolPumplet(WarGrey::SCADA::CoolPumpStatus default_status, float radiusX, float radiusY = 0.0F, double degrees = 0.0);
		CoolPumplet(float radiusX, float radiusY = 0.0F, double degrees = 0.0);

	public:
		void construct() override;
		void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void fill_pump_origin(float* x = nullptr, float* y = nullptr);

	protected:
		void prepare_style(WarGrey::SCADA::CoolPumpStatus status, WarGrey::SCADA::CoolPumpStyle& style) override;
		
	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ inlet;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ indicator;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ iborder;
		
	private:
		Windows::Foundation::Rect enclosing_box;
		float radius;
		float pump_cx;
		float pump_cy;
		bool leftward;
		bool upward;
	};
}
