#pragma once

#include "graphlet/primitive.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Booleanlet : public virtual WarGrey::SCADA::IScalelet<bool> {
	public:
		Booleanlet(float size, Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ true_color,
			Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ false_color = WarGrey::SCADA::Colours::WhiteSmoke);

	public:
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	private:
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ true_color;
		Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ false_color;

	private:
		float size;
		float gapsize;
	};
}
