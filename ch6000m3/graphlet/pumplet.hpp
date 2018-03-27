#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class PumpState { Running, Starting, Unstartable, Remote, Stopped, Stopping, Unstoppable, Ready, _ };

	private struct PumpStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ effect_color;
	};

	WarGrey::SCADA::PumpStyle make_default_pump_style(WarGrey::SCADA::PumpState state);

	private class Pumplet : public WarGrey::SCADA::IStatelet<WarGrey::SCADA::PumpState, WarGrey::SCADA::PumpStyle> {
	public:
		Pumplet(WarGrey::SCADA::PumpState default_state, float radius, double degree = -90.0);
		Pumplet(float radius, double degree = -90.0);

	public:
		void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		double get_direction_degree();

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		
	private:
		double degree;
		float size;
		float thickness;
	};
}
