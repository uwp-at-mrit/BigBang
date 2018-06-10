#pragma once

#include "graphlet/primitive.hpp"
#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Indicatorlet : public WarGrey::SCADA::IValuelet<float> {
	public:
		Indicatorlet(float size, float thickness = 0.0F,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ low_color = WarGrey::SCADA::Colours::make(0x30A1F6),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ normal_color = WarGrey::SCADA::Colours::make(0xAEEE00),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ high_color = WarGrey::SCADA::Colours::make(0xFFB43D),
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = WarGrey::SCADA::Colours::make(0x505050));

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void fill_margin(float x, float y, float* t = nullptr, float* r = nullptr, float* b = nullptr, float* l = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Geometry::CanvasCachedGeometry^ body_ring;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ normal_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ low_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ high_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;

	private:
		float bspace;
		float size;
		float thickness;
	};
}
