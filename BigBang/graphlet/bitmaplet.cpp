#include <ppltasks.h>
#include <map>

#include "graphlet/bitmaplet.hpp"
#include "planet.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

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
	this->ms_appx_bmp = ms_appx_path(file, ".png", rootdir);
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
BitmapBooleanlet::BitmapBooleanlet(Platform::String^ t_file, Platform::String^ f_file, Platform::String^ rootdir)
	: BitmapBooleanlet(t_file, f_file, 0.0F, 0.0F, rootdir) {}

BitmapBooleanlet::BitmapBooleanlet(Platform::String^ subdir, Platform::String^ rootdir)
	: BitmapBooleanlet(subdir, 0.0F, 0.0F, rootdir) {}

BitmapBooleanlet::BitmapBooleanlet(Platform::String^ subdir, float width, float height, Platform::String^ rootdir)
	: BitmapBooleanlet(subdir + "/true", subdir + "/false", width, height, rootdir) {}

BitmapBooleanlet::BitmapBooleanlet(Platform::String^ t_file, Platform::String^ f_file, float width, float height, Platform::String^ rootdir) {
	this->window.Width = width;
	this->window.Height = height;
	this->ms_appx_tmp = ms_appx_path(t_file, ".png", rootdir);
	this->ms_appx_fmp = ms_appx_path(f_file, ".png", rootdir);
}

BitmapBooleanlet::~BitmapBooleanlet() {
	this->unload(this->ms_appx_tmp);
	this->unload(this->ms_appx_fmp);
}

void BitmapBooleanlet::construct() {
	this->load(this->ms_appx_tmp, true);
	this->load(this->ms_appx_fmp, false);
}

void BitmapBooleanlet::on_appx(Uri^ ms_appx, CanvasBitmap^ doc_bmp, bool hint) {
	if (hint) {
		this->graph_tmp = doc_bmp;
	} else {
		this->graph_fmp = doc_bmp;
	}

	// NOTE: The client application should guarantee that source bitmaps have the same size
	adjust_window_size(this->window, doc_bmp);
}

bool BitmapBooleanlet::ready() {
	if (this->get_value() == true) {
		return (this->graph_tmp != nullptr);
	} else {
		return (this->graph_fmp != nullptr);
	}
}

void BitmapBooleanlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->window.Width, h, this->window.Height);
}

void BitmapBooleanlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	auto target = (this->get_value() ? this->graph_tmp : this->graph_fmp);
	this->window.X = x;
	this->window.Y = y;

	ds->DrawImage(target, this->window);
}

void BitmapBooleanlet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Uri^ target = (this->get_value() ? this->ms_appx_tmp : this->ms_appx_fmp);
	Platform::String^ hint = file_name_from_path(target);

	draw_invalid_bitmap(hint, ds, x, y, this->window.Width, this->window.Height);
}
