#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class AccumulatorStatus { Normal, Breakdown, _ };

	private struct AccumulatorStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
	};

	WarGrey::SCADA::AccumulatorStyle make_default_accumulator_style(WarGrey::SCADA::AccumulatorStatus status);

	private class Accumulatorlet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::AccumulatorStatus, WarGrey::SCADA::AccumulatorStyle> {
	public:
		Accumulatorlet(WarGrey::SCADA::AccumulatorStatus default_status, float radius, float thickness = 1.5F, double degrees = 0.0);
		Accumulatorlet(float radius, float thickness = 1.5F, double degrees = 0.0);

	public:
		void construct() override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ skeleton;

	private:
		Windows::Foundation::Rect electricity;
		float thickness;
	};
}
