#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class ConverterStatus { Normal, Breakdown, _ };

	private struct ConverterStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
	};

	private class Converterlet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::ConverterStatus, WarGrey::SCADA::ConverterStyle> {
	public:
		Converterlet(WarGrey::SCADA::ConverterStatus default_status, Platform::String^ sign, float radius, float thickness = 1.5F, double degrees = 0.0);
		Converterlet(Platform::String^ sign, float radius, float thickness = 1.5F, double degrees = 0.0);

	public:
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::ConverterStatus status, ConverterStyle& style) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ sign;

	private:
		float thickness;
	};
}
