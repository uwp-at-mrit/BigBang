#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class HopperPumpStatus {
		Running, Unstartable, Stopped, Unstoppable, _
	};

	private struct HopperPumpStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
	};

	private class HopperPumplet
		: public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::HopperPumpStatus, WarGrey::SCADA::HopperPumpStyle> {
	public:
		HopperPumplet(WarGrey::SCADA::HopperPumpStatus default_status, float radiusX, float radiusY = 0.0F, double degrees = 0.0);
		HopperPumplet(float radiusX, float radiusY = 0.0F, double degrees = 0.0);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::HopperPumpStatus status, WarGrey::SCADA::HopperPumpStyle& style) override;
		
	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ inlet;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ indicator;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ iborder;
		
	private:
		float radius;
		bool leftward;
		bool upward;
	};
}
