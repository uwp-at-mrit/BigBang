#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class ConverterState { Normal, Breakdown, _ };

	private struct ConverterStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
	};

	private class Converterlet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::ConverterState, WarGrey::SCADA::ConverterStyle> {
	public:
		Converterlet(WarGrey::SCADA::ConverterState default_state, char sign, float radius, float thickness = 1.5F, double degrees = 0.0);
		Converterlet(char sign, float radius, float thickness = 1.5F, double degrees = 0.0);

	public:
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::ConverterState status, ConverterStyle& style) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ sign;

	private:
		float thickness;
	};
}
