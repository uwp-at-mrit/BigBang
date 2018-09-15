#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class ManualValveStatus {
		Disabled,
		Open, Unopenable, OpenReady,
		Closed, Unclosable, CloseReady,
		_
	};

	private struct ManualValveStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ stem_color;
	};

	private class ManualValvelet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::ManualValveStatus, WarGrey::SCADA::ManualValveStyle> {
	public:
		ManualValvelet(WarGrey::SCADA::ManualValveStatus default_status, float radius, double degrees = 0.0);
		ManualValvelet(float radius, double degrees = 0.0);

	public:
		void construct() override;
		void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		void prepare_style(WarGrey::SCADA::ManualValveStatus status, WarGrey::SCADA::ManualValveStyle& style) override;
		void on_status_changed(WarGrey::SCADA::ManualValveStatus status) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bottom_up_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ top_down_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ bottom_up_ready_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ top_down_ready_mask;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ skeleton;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ stem;
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body;
	};
}
