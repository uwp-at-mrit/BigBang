#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class PumpState { Running, Starting, Unstartable, Remote, Stopped, Stopping, Unstoppable, Ready, _ };

	private struct PumpStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
	};

	WarGrey::SCADA::PumpStyle make_default_pump_style(WarGrey::SCADA::PumpState state);

	private class Pumplet : public WarGrey::SCADA::IStatelet<WarGrey::SCADA::PumpState, WarGrey::SCADA::PumpStyle> {
	public:
		Pumplet(WarGrey::SCADA::PumpState default_state, float radius, double degrees = -90.0);
		Pumplet(float radius, double degrees = -90.0);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		double get_direction_degrees();

	protected:
		void on_state_change(PumpState state) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ unstartable_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ unstoppable_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		
	private:
		double degrees;
		float tradius;
		float size;
		float thickness;

	private:
		double mask_percentage;
	};
}
