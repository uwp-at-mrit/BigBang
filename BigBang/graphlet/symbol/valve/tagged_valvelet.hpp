#pragma once

#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class TValveStatus {
		Disabled,
		Open, Opening, Unopenable, OpenReady,
		Closed, Closing, Unclosable, CloseReady,
		FakeOpen, FakeClose,
		_
	};

	private struct TValveStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ frame_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ body_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ skeleton_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ mask_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ tag_color;
	};

	private class TValvelet abstract
		: public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::TValveStatus, WarGrey::SCADA::TValveStyle> {
	public:
		TValvelet(WarGrey::SCADA::TValveStatus default_status, char tag, float radius, double degrees = 0.0, bool rotate_tag = true);
		TValvelet(char tag, float radius, double degrees = 0.0, bool rotate_tag = true);

	public:
		void construct() override;
		void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr) override;
		void update(long long count, long long interval, long long uptime) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void fill_valve_origin(float* x, float* y);

	protected:
		void prepare_style(WarGrey::SCADA::TValveStatus status, WarGrey::SCADA::TValveStyle& style) override;
		void on_status_changed(WarGrey::SCADA::TValveStatus status) override;

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
		Windows::Foundation::Rect enclosing_box;
		float sgradius;
		float sradius;
		float fradius;

	private:
		Platform::String^ tag;
		bool rotate_tag;
		float tag_xoff;
		float tag_yoff;
		float sign_cx;
		float sign_cy;
		float body_cx;
		float body_cy;

	private:
		double mask_percentage;
	};

	private class MotorValvelet : public WarGrey::SCADA::TValvelet {
	public:
		MotorValvelet(WarGrey::SCADA::TValveStatus default_status, float radius, double degrees = 0.0, bool rotate_tag = true);
		MotorValvelet(float radius, double degrees = 0.0, bool rotate_tag = true);
	};
}
