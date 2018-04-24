#pragma once

#include "brushes.hxx"
#include "text.hpp"

void draw_invalid_bitmap(Platform::String^ hint, Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds,
	float x, float y, float width, float height, float thickness = 2.0F,
	Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color = WarGrey::SCADA::Colours::GrayText,
	Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ font = make_text_format());
