#include "graphlet/bitmaplet.hpp"
#include "planet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::Storage;
using namespace Windows::Storage::Streams;

using namespace Microsoft::Graphics::Canvas;

void WarGrey::SCADA::adjust_window_size(Rect& window, CanvasBitmap^ doc_bmp) {
	if (window.Width <= 0.0F) {
		if (window.Height <= 0.0F) {
			window.Height = doc_bmp->Size.Height;
		}
		window.Width = doc_bmp->Size.Width * (window.Height / doc_bmp->Size.Height);
	} else if (window.Height <= 0.0F) {
		window.Height = doc_bmp->Size.Height * (window.Width / doc_bmp->Size.Width);
	}
}

/*************************************************************************************************/
Bitmaplet::Bitmaplet(Platform::String^ file, Platform::String^ rootdir) : Bitmaplet(file, 0.0F, 0.0F, rootdir) {}

Bitmaplet::Bitmaplet(Platform::String^ file, float width, float height, Platform::String^ rootdir) {
	this->window.Width = width;
	this->window.Height = height;
	this->ms_appx_bmp = ms_appx_file(file, ".png", rootdir);
}

Bitmaplet::~Bitmaplet() {
	this->unload(this->ms_appx_bmp);
}

void Bitmaplet::construct() {
	this->load(this->ms_appx_bmp, 0);
}

void Bitmaplet::on_appx(Uri^ ms_appx, CanvasBitmap^ doc_bmp, int hint) {
	this->graph_bmp = doc_bmp;
	adjust_window_size(this->window, doc_bmp);
}

bool Bitmaplet::ready() {
	return (this->graph_bmp != nullptr);
}

void Bitmaplet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->window.Width, h, this->window.Height);
}

void Bitmaplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	this->window.X = x;
	this->window.Y = y;

	ds->DrawImage(this->graph_bmp, this->window);
}

void Bitmaplet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Platform::String^ hint = file_name_from_path(this->ms_appx_bmp);

	draw_invalid_bitmap(hint, ds, x, y, this->window.Width, this->window.Height);
}

/*************************************************************************************************/
OptionBitmaplet::OptionBitmaplet(Platform::String^ t_file, Platform::String^ f_file, Platform::String^ rootdir)
	: OptionBitmaplet(t_file, f_file, 0.0F, 0.0F, rootdir) {}

OptionBitmaplet::OptionBitmaplet(Platform::String^ subdir, Platform::String^ rootdir)
	: OptionBitmaplet(subdir, 0.0F, 0.0F, rootdir) {}

OptionBitmaplet::OptionBitmaplet(Platform::String^ subdir, float width, float height, Platform::String^ rootdir)
	: OptionBitmaplet(subdir + "/true", subdir + "/false", width, height, rootdir) {}

OptionBitmaplet::OptionBitmaplet(Platform::String^ t_file, Platform::String^ f_file, float width, float height, Platform::String^ rootdir) {
	this->window.Width = width;
	this->window.Height = height;
	this->ms_appx_tmp = ms_appx_file(t_file, ".png", rootdir);
	this->ms_appx_fmp = ms_appx_file(f_file, ".png", rootdir);
}

OptionBitmaplet::~OptionBitmaplet() {
	this->unload(this->ms_appx_tmp);
	this->unload(this->ms_appx_fmp);
}

void OptionBitmaplet::construct() {
	this->load(this->ms_appx_tmp, true);
	this->load(this->ms_appx_fmp, false);
}

void OptionBitmaplet::on_appx(Uri^ ms_appx, CanvasBitmap^ doc_bmp, bool hint) {
	if (hint) {
		this->graph_tmp = doc_bmp;
	} else {
		this->graph_fmp = doc_bmp;
	}

	// NOTE: The client application should guarantee that source bitmaps have the same size
	adjust_window_size(this->window, doc_bmp);
}

bool OptionBitmaplet::ready() {
	/** NOTE
	 * By design, the absence of `false bitmap` indicates that the false status is meaningless.
	 * If the client applications do care about the false status, they should use the `UnionBitmaplet` instead. 
	 */
	return (this->graph_tmp != nullptr);
}

void OptionBitmaplet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->window.Width, h, this->window.Height);
}

void OptionBitmaplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	auto target = (this->get_value() ? this->graph_tmp : this->graph_fmp);

	if (target != nullptr) {
		this->window.X = x;
		this->window.Y = y;

		ds->DrawImage(target, this->window);
	}
}

void OptionBitmaplet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Platform::String^ hint = file_name_from_path(this->ms_appx_tmp);

	draw_invalid_bitmap(hint, ds, x, y, this->window.Width, this->window.Height);
}
