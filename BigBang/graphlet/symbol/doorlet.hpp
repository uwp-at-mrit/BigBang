#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class DoorStatus { Open, Opening, Closed, Closing, Disabled, _ };

	private struct DoorStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ handler_color;
	};

	private class BottomDoorlet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::DoorStatus, WarGrey::SCADA::DoorStyle> {
	public:
		BottomDoorlet(WarGrey::SCADA::DoorStatus default_state, float radius, double degrees = -90.0);
		BottomDoorlet(float radius, double degrees = -90.0);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::DoorStatus status, WarGrey::SCADA::DoorStyle& style) override;
		void on_status_changed(WarGrey::SCADA::DoorStatus state) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bottom_up_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ top_down_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bottom_up_ready_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ top_down_ready_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ frame;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ handle;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		
	private:
		float sgradius;
		float fradius;

	private:
		double mask_percentage;
	};

	private class UpperHopperDoorlet : public WarGrey::SCADA::BottomDoorlet {
		using BottomDoorlet::BottomDoorlet;
	};
}
