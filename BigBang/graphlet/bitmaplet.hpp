#pragma once

#include "graphlet/primitive.hpp"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Bitmaplet : public virtual WarGrey::SCADA::IGraphlet {
	public:
		~Bitmaplet() noexcept;

		Bitmaplet(Platform::String^ file_bmp, float width = 0.0F, float height = 0.0F, Platform::String^ rootdir = "graphlet");
		Bitmaplet(Platform::String^ file_bmp, Platform::String^ rootdir);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;

	protected:
		virtual void draw_on_error(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height);

	protected:
		Microsoft::Graphics::Canvas::CanvasBitmap^ graph_bmp;

	protected:
		Windows::Foundation::Rect viewport;
		Windows::Foundation::Uri^ ms_appx_bmp;
	};
}
	