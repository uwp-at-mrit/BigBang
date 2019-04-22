#pragma once

#include <map>

#include "graphlet/msappxlet.hxx"

#include "datum/path.hpp"

#include "brushes.hxx"
#include "draw.hpp"

namespace WarGrey::SCADA {
	void adjust_window_size(Windows::Foundation::Rect& window, Microsoft::Graphics::Canvas::CanvasBitmap^ doc_bmp);

	private class Bitmaplet : public virtual WarGrey::SCADA::IMsAppxlet<Microsoft::Graphics::Canvas::CanvasBitmap, int> {
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
		void on_appx(Windows::Foundation::Uri^ ms_appx_bmp, Microsoft::Graphics::Canvas::CanvasBitmap^ doc_bmp, int hint) override;
		void on_appx_not_found(Windows::Foundation::Uri^ ms_appx_bmp, int hint) override {}

	protected:
		Microsoft::Graphics::Canvas::CanvasBitmap^ graph_bmp;

	protected:
		Windows::Foundation::Rect window;
		Windows::Foundation::Uri^ ms_appx_bmp;
	};

	private class OptionBitmaplet
		: public virtual WarGrey::SCADA::IMsAppxlet<Microsoft::Graphics::Canvas::CanvasBitmap, bool>
		, public virtual WarGrey::SCADA::IValuelet<bool> {
	public:
		virtual ~OptionBitmaplet() noexcept;

		OptionBitmaplet(Platform::String^ subdir, float width = 0.0F, float height = 0.0F, Platform::String^ rootdir = "graphlet");
		OptionBitmaplet(Platform::String^ subdir, Platform::String^ rootdir);

		OptionBitmaplet(Platform::String^ file_tmp, Platform::String^ file_fmp, float width, float height, Platform::String^ rootdir = "graphlet");
		OptionBitmaplet(Platform::String^ file_tmp, Platform::String^ file_fmp, Platform::String^ rootdir);

	public:
		void construct() override;
		void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override;
		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override;
		bool ready() override;

	protected:
		void on_appx(Windows::Foundation::Uri^ ms_appx_bmp, Microsoft::Graphics::Canvas::CanvasBitmap^ doc_bmp, bool hint) override;

	protected:
		Microsoft::Graphics::Canvas::CanvasBitmap^ graph_tmp;
		Microsoft::Graphics::Canvas::CanvasBitmap^ graph_fmp;

	protected:
		Windows::Foundation::Rect window;
		Windows::Foundation::Uri^ ms_appx_tmp;
		Windows::Foundation::Uri^ ms_appx_fmp;
	};

	template <typename U>
	private class UnionBitmaplet
		: public virtual WarGrey::SCADA::IMsAppxlet<Microsoft::Graphics::Canvas::CanvasBitmap, U>
		, public virtual WarGrey::SCADA::IValuelet<U> {
	public:
		virtual ~UnionBitmaplet() noexcept {
			for (U s = _E0(U); s < U::_; s++) {
				this->unload(this->ms_appx_bmps[s]);
			}
		}

		UnionBitmaplet(float width = 0.0F, float height = 0.0F, Platform::String^ rootdir = "graphlet")
			: UnionBitmaplet(nullptr, widht, height, rootdir) {}

		UnionBitmaplet(Platform::String^ subdir, float width = 0.0F, float height = 0.0F, Platform::String^ rootdir = "graphlet") {
			this->window.Width = width;
			this->window.Height = height;

			for (U s = _E0(U); s < U::_; s++) {
				Platform::String^ file_bmp = ((subdir == nullptr) ? s.ToString() : subdir + "/" + s.ToString());

				this->ms_appx_bmps[s] = ms_appx_file(file_bmp, ".png", rootdir);
			}
		}

	public:
		void construct() override {
			for (U s = _E0(U); s < U::_; s++) {
				this->load(this->ms_appx_bmps[s], s);
			}
		}

		void fill_extent(float x, float y, float* w, float* h) override {
			SET_VALUES(w, this->window.Width, h, this->window.Height);
		}

		void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {
			this->window.X = x;
			this->window.Y = y;

			ds->DrawImage(this->graph_bmps[this->get_value()], this->window);
		}

		void draw_progress(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {
			Platform::String^ hint = file_name_from_path(this->ms_appx_bmps[this->get_value()]);

			draw_invalid_bitmap(hint, ds, x, y, this->window.Width, this->window.Height);
		}

		bool ready() override {
			return (this->graph_bmps[this->get_value()] != nullptr);
		}

	public:
		void on_appx(Windows::Foundation::Uri^ ms_appx, Microsoft::Graphics::Canvas::CanvasBitmap^ doc_bmp, U hint) override {
			this->graph_bmps[hint] = doc_bmp;

			// NOTE: The client application should guarantee that source bitmaps have the same size
			adjust_window_size(this->window, doc_bmp);
		}

	protected:
		Windows::Foundation::Rect window;
		std::map<U, Windows::Foundation::Uri^> ms_appx_bmps;
		std::map<U, Microsoft::Graphics::Canvas::CanvasBitmap^> graph_bmps;
	};
}