#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class PowerStationState { Normal, Breakdown, _ };

	private struct PowerStationStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
	};

	private class PowerStationlet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::PowerStationState, WarGrey::SCADA::PowerStationStyle> {
	public:
		PowerStationlet(WarGrey::SCADA::PowerStationState default_state, float radius, float thickness = 1.5F, double degrees = 0.0);
		PowerStationlet(float radius, float thickness = 1.5F, double degrees = 0.0);

	public:
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		
	protected:
		void prepare_style(WarGrey::SCADA::PowerStationState status, WarGrey::SCADA::PowerStationStyle& style) override;

	private:
		float thickness;
		float ring_radius;
		float cx1;
		float cy1;
		float cx2;
		float cy2;
	};
}
