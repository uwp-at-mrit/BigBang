#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class ValveState {
		Manual,
		Open, Opening, Unopenable, ConditionalOpen,
		Closed, Closing, Unclosable, ConditionalClose,
		FalseOpen, FalseClose,
		_ };

	private struct ValveStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
	};

	WarGrey::SCADA::ValveStyle make_default_valve_style(WarGrey::SCADA::ValveState state);

	private class Valvelet : public WarGrey::SCADA::IStatelet<WarGrey::SCADA::ValveState, WarGrey::SCADA::ValveStyle> {
	public:
		Valvelet(WarGrey::SCADA::ValveState default_state, float radius, double degrees = -90.0);
		Valvelet(float radius, double degrees = -90.0);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		double get_direction_degrees();

	protected:
		void on_state_change(ValveState state) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ unopenable_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ unclosable_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ manual_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ frame;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		
	private:
		double degrees;
		float sgradius;
		float fradius;
		float size;
		float thickness;

	private:
		double mask_percentage;
	};
}
