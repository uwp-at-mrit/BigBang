#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class MachineStatus { Normal, Breakdown, _ };

	private struct MachineStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color;
	};

	WarGrey::SCADA::MachineStyle make_default_machine_style(WarGrey::SCADA::MachineStatus status);

	private class IMachinelet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::MachineStatus, WarGrey::SCADA::MachineStyle> {
	public:
		IMachinelet(WarGrey::SCADA::MachineStatus default_status, unsigned char label, float radius, float thickness = 1.5F, double degrees = 0.0);
		IMachinelet(unsigned char label, float radius, float thickness = 1.5F, double degrees = -90.0);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ shape;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ caption;
		
	protected:
		unsigned char mark;
		float thickness;
	};
}
