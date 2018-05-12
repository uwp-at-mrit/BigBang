#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class SolarPanelStatus { Normal, Breakdown, _ };

	private struct SolarPanelStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ bgcolor;
	};

	WarGrey::SCADA::SolarPanelStyle make_default_solarpanel_style(WarGrey::SCADA::SolarPanelStatus status);

	private class SolarPanellet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::SolarPanelStatus, WarGrey::SCADA::SolarPanelStyle> {
	public:
		SolarPanellet(WarGrey::SCADA::SolarPanelStatus default_status, float radius, float thickness = 1.5F, double degrees = 0.0);
		SolarPanellet(float radius, float thickness = 1.5F, double degrees = 0.0);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;

	private:
		float thickness;
	};
}
