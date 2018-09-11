#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class ValveStatus {
		Manual,
		Open, Opening, Unopenable, OpenReady,
		Closed, Closing, Unclosable, CloseReady,
		FakeOpen, FakeClose,
		_ };

	private struct ValveStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ frame_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ handle_color;
	};

	private class Valvelet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::ValveStatus, WarGrey::SCADA::ValveStyle> {
	public:
		Valvelet(WarGrey::SCADA::ValveStatus default_status, float radius, double degrees = -90.0);
		Valvelet(float radius, double degrees = -90.0);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::ValveStatus status, WarGrey::SCADA::ValveStyle& style) override;
		void on_status_changed(WarGrey::SCADA::ValveStatus status) override;

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

	private class TValvelet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::ValveStatus, WarGrey::SCADA::ValveStyle> {
	public:
		TValvelet(WarGrey::SCADA::ValveStatus default_status, char tag, float radius, double degrees = -90.0);
		TValvelet(char tag, float radius, double degrees = -90.0);
		
		TValvelet(WarGrey::SCADA::ValveStatus default_status, float radius, double degrees = -90.0);
		TValvelet(float radius, double degrees = -90.0);

	public:
		void construct() override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::ValveStatus status, WarGrey::SCADA::ValveStyle& style) override;
		void on_status_changed(WarGrey::SCADA::ValveStatus status) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bottom_up_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ top_down_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bottom_up_ready_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ top_down_ready_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ frame;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ sign_body;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ sign;

	private:
		float sgradius;
		float sradius;
		float fradius;

	private:
		double mask_percentage;

	private:
		Platform::String^ tag;
		float tag_xoff;
		float tag_yoff;
	};
}
