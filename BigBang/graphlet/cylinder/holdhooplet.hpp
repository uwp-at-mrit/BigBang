#pragma once

#include "graphlet/primitive.hpp"

#include "paint.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private enum class HoldHoopState { Held, Loose, _ };

	private struct HoldHoopStyle {
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ border_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ hoop_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ running_color;
	};

	private class HoldHooplet : public WarGrey::SCADA::ISymbollet<WarGrey::SCADA::HoldHoopState, WarGrey::SCADA::HoldHoopStyle> {
	public:
		HoldHooplet(float radius, double degrees = 0.0F);
		HoldHooplet(WarGrey::SCADA::HoldHoopState default_state, float radius, double degrees = 0.0F);

	public:
		void construct() override;
		void fill_margin(float x, float y, float* top = nullptr, float* right = nullptr, float* bottom = nullptr, float* left = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void set_running(bool yes_no);
		void fill_hoop_origin(float* cx, float* cy);
		void fill_cylinder_origin(float* cx, float* cy);

	protected:
		void prepare_style(WarGrey::SCADA::HoldHoopState status, WarGrey::SCADA::HoldHoopStyle& style) override;
		void on_state_changed(WarGrey::SCADA::HoldHoopState status) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ cylinder;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ held_hoop;
		Microsoft::Graphics::Canvas::Geometry::CanvasGeometry^ loose_hoop;

	private:
		float cylinder_cx;
		float cylinder_cy;

	private:
		Windows::Foundation::Rect enclosing_box;
		bool running;
	};
}
