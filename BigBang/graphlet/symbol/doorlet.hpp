#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class DoorStatus { Open, Opening, Closed, Closing, Disabled, _ };

	private struct DoorStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ door_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ disable_color;
	};

	private class BottomDoorlet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::DoorStatus, WarGrey::SCADA::DoorStyle> {
	public:
		BottomDoorlet(WarGrey::SCADA::DoorStatus default_state, float radius, double degrees = 0.0);
		BottomDoorlet(float radius, double degrees = 0.0);

	public:
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::DoorStatus status, WarGrey::SCADA::DoorStyle& style) override;
		void on_status_changed(WarGrey::SCADA::DoorStatus state) override;

	private:
		void make_masked_door_partitions();

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ disable_line;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ door_partitions[3];
		
	private:
		float radius;

	private:
		double mask_percentage;
	};

	private class UpperHopperDoorlet : public WarGrey::SCADA::BottomDoorlet {
		using BottomDoorlet::BottomDoorlet;
	};
}
