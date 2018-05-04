#pragma once

#include "graphlet/msappxlet.hxx"

#include "brushes.hxx"

namespace WarGrey::SCADA {
	private class Bitmaplet : public virtual WarGrey::SCADA::IMsAppxlet<Microsoft::Graphics::Canvas::CanvasBitmap> {
	public:
		virtual ~Bitmaplet() noexcept;

		Bitmaplet(Platform::String^ file_bmp, float width = 0.0F, float height = 0.0F, Platform::String^ rootdir = "graphlet");
		Bitmaplet(Platform::String^ file_bmp, Platform::String^ rootdir);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready() override;

	protected:
		void on_appx(Windows::Foundation::Uri^ ms_appx_bmp, Microsoft::Graphics::Canvas::CanvasBitmap^ doc_bmp) override;
		void on_appx_not_found(Windows::Foundation::Uri^ ms_appx_bmp) override {}

	protected:
		Microsoft::Graphics::Canvas::CanvasBitmap^ graph_bmp;

	protected:
		Windows::Foundation::Rect window;
		Windows::Foundation::Uri^ ms_appx_bmp;
	};

	private class BitmapBooleanlet
		: public virtual WarGrey::SCADA::IMsAppxlet<Microsoft::Graphics::Canvas::CanvasBitmap>
		, public virtual WarGrey::SCADA::IValuelet<bool> {
	public:
		virtual ~BitmapBooleanlet() noexcept;

		BitmapBooleanlet(Platform::String^ file_prefix, float width = 0.0F, float height = 0.0F, Platform::String^ rootdir = "graphlet");
		BitmapBooleanlet(Platform::String^ file_prefix, Platform::String^ rootdir);

		BitmapBooleanlet(Platform::String^ file_tmp, Platform::String^ file_fmp, float width = 0.0F, float height = 0.0F, Platform::String^ rootdir = "graphlet");
		BitmapBooleanlet(Platform::String^ file_tmp, Platform::String^ file_fmp, Platform::String^ rootdir);


	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready() override;

	protected:
		void on_appx(Windows::Foundation::Uri^ ms_appx_bmp, Microsoft::Graphics::Canvas::CanvasBitmap^ doc_bmp) override;
		
	protected:
		Microsoft::Graphics::Canvas::CanvasBitmap^ graph_tmp;
		Microsoft::Graphics::Canvas::CanvasBitmap^ graph_fmp;

	protected:
		Windows::Foundation::Rect window;
		Windows::Foundation::Uri^ ms_appx_tmp;
		Windows::Foundation::Uri^ ms_appx_fmp;
	};
}
	