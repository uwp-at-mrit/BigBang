#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class PumpStatus { Running, Starting, Unstartable, Remote, Stopped, Stopping, Unstoppable, Ready, _ };

	private struct PumpStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
	};

	private class Pumplet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::PumpStatus, WarGrey::SCADA::PumpStyle> {
	public:
		Pumplet(WarGrey::SCADA::PumpStatus default_status, float radius, double degrees = -90.0);
		Pumplet(float radius, double degrees = -90.0);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::PumpStatus status, WarGrey::SCADA::PumpStyle& style) override;
		void on_status_change(PumpStatus status) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ unstartable_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ unstoppable_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		
	private:
		float tradius;

	private:
		double mask_percentage;
	};
}
