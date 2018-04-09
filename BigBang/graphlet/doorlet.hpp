#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class DoorState { Open, Opening, Closed, Closing, Disabled, _ };

	private struct DoorStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ handler_color;
	};

	WarGrey::SCADA::DoorStyle make_default_door_style(WarGrey::SCADA::DoorState state);

	private class HopperDoorlet : public WarGrey::SCADA::IStatelet<WarGrey::SCADA::DoorState, WarGrey::SCADA::DoorStyle> {
	public:
		HopperDoorlet(WarGrey::SCADA::DoorState default_state, float radius, double degrees = -90.0);
		HopperDoorlet(float radius, double degrees = -90.0);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* width = nullptr, float* height = nullptr) override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		double get_direction_degrees();

	protected:
		void on_state_change(DoorState state) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bottom_up_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ top_down_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bottom_up_ready_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ top_down_ready_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ frame;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ handler;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		
	private:
		double degrees;
		float sgradius;
		float fradius;
		float size;

	private:
		double mask_percentage;
	};

	private class Doorlet : public WarGrey::SCADA::HopperDoorlet {
		using HopperDoorlet::HopperDoorlet;
	};
}
