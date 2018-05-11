#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class MachineStatus { Normal, Breakdown, _ };

	private struct MachineStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ label_color;
	};

	WarGrey::SCADA::MachineStyle make_default_machine_style(WarGrey::SCADA::MachineStatus status);

	private class Machinelet : public virtual WarGrey::SCADA::IStatuslet<WarGrey::SCADA::MachineStatus, WarGrey::SCADA::MachineStyle> {
	public:
		Machinelet(WarGrey::SCADA::MachineStatus default_status, float radius, float thickness = 1.5F, double degrees = 0.0);
		Machinelet(float radius, float thickness = 1.5F, double degrees = -90.0);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		double get_direction_degrees();

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ shape;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ caption;
		
	private:
		double degrees;
		float thickness;
		float size;
	};
}
