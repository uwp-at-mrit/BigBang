#pragma once

#include "graphlet/primitive.hpp"

#include "palette.hpp"
#include "satellite.hpp"

namespace WarGrey::DTPM {
	private class ColorPickerlet : public WarGrey::SCADA::IGraphlet {
	public:
		ColorPickerlet(WarGrey::DTPM::Palette ptype, float width = 128.0F, float height = 32.0F);
		ColorPickerlet(WarGrey::DTPM::Palette ptype, Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ initial_color = nullptr,
			float width = 128.0F, float height = 32.0F);
		
	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	public:
		void own_caret(bool is_own) override;

	public:
		void color(Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color);
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color();

	private:
		Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ _color;
		WarGrey::DTPM::Palette ptype;
		WarGrey::SCADA::ISatellite* picker;

	private:
		float width;
		float height;
	};
}
