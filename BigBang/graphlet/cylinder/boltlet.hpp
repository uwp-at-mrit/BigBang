#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class BoltState { SlidedIn, SlidedOut, _ };

	private struct BoltStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ bolt_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ running_color;
	};

	private class Boltlet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::BoltState, WarGrey::SCADA::BoltStyle> {
	public:
		Boltlet(float radius, double degrees = 0.0F);
		Boltlet(WarGrey::SCADA::BoltState default_state, float radius, double degrees = 0.0F);

	public:
		void construct() override;
		void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_running(bool yes_no);

	protected:
		void prepare_style(WarGrey::SCADA::BoltState status, WarGrey::SCADA::BoltStyle& style) override;
		void on_state_changed(WarGrey::SCADA::BoltState status) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cylinder;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ slide_in;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ slide_out;

	private:
		Windows::Foundation::Rect enclosing_box;
		bool running;
	};
}
