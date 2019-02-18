#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class MachineState { Normal, Breakdown, _ };

	private struct MachineStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ sign_color;
	};

	private class Machinelet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::MachineState, WarGrey::SCADA::MachineStyle> {
	public:
		Machinelet(WarGrey::SCADA::MachineState default_state, char sign, float radius, float thickness = 1.5F, double degrees = 0.0);
		Machinelet(char sign, float radius, float thickness = 1.5F, double degrees = 0.0);

	public:
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::MachineState status, WarGrey::SCADA::MachineStyle& style) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ sign;
		
	private:
		float thickness;
	};
}
