#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class HydraulicPumpState {
		Running, StartReady, Starting, Unstartable, Stopped, StopReady, Stopping, Unstoppable, Ready, Broken, _
	};

	private struct HydraulicPumpStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ remote_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
	};

	private class HydraulicPumplet
		: public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::HydraulicPumpState, WarGrey::SCADA::HydraulicPumpStyle> {
	public:
		HydraulicPumplet(WarGrey::SCADA::HydraulicPumpState default_state, float radius, double degrees = 0.0);
		HydraulicPumplet(float radius, double degrees = 0.0);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_remote_control(bool on);

	protected:
		void prepare_style(WarGrey::SCADA::HydraulicPumpState status, WarGrey::SCADA::HydraulicPumpStyle& style) override;
		void on_status_changed(HydraulicPumpState status) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ start_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ stop_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		
	private:
		float tradius;

	private:
		bool remote_control;
	};
}
