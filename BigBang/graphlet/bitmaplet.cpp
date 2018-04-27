#include <ppltasks.h>
#include <map>

#include "graphlet/bitmaplet.hpp"
#include "planet.hpp"

#include "colorspace.hpp"
#include "path.hpp"
#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;

using namespace Windows::Storage;
using namespace Windows::Storage::Streams;

using namespace Microsoft::Graphics::Canvas;

static std::map<int, cancellation_token_source> lazy_tasks;

/*************************************************************************************************/
Bitmaplet::Bitmaplet(Platform::String^ file, Platform::String^ rootdir) : Bitmaplet(file, 0.0F, 0.0F, rootdir) {}

Bitmaplet::Bitmaplet(Platform::String^ file, float width, float height, Platform::String^ rootdir) {
	this->window.Width = width;
	this->window.Height = height;
	this->ms_appx_bmp = ms_appx_path(file, rootdir, ".png");
}

Bitmaplet::~Bitmaplet() {
	int uuid = this->ms_appx_bmp->GetHashCode();
	auto t = lazy_tasks.find(uuid);

	if (t != lazy_tasks.end()) {
		if (this->graph_bmp == nullptr) {
			t->second.cancel();
		}

		lazy_tasks.erase(t);
	}
}

void Bitmaplet::construct() {
	int uuid = this->ms_appx_bmp->GetHashCode();
	cancellation_token_source bmp_task;

	lazy_tasks.insert(std::pair<int, cancellation_token_source>(uuid, bmp_task));
	this->load_async(this->ms_appx_bmp, bmp_task.get_token());
}

void Bitmaplet::on_appx(Uri^ ms_appx, CanvasBitmap^ doc_bmp) {
	this->graph_bmp = doc_bmp;

	if (this->window.Width <= 0.0F) {
		if (this->window.Height <= 0.0F) {
			this->window.Height = this->graph_bmp->Size.Height;
		}
		this->window.Width = this->graph_bmp->Size.Width * (this->window.Height / this->graph_bmp->Size.Height);
	} else if (this->window.Height <= 0.0F) {
		this->window.Height = this->graph_bmp->Size.Height * (this->window.Width / this->graph_bmp->Size.Width);
	}
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

BitmapBooleanlet::BitmapBooleanlet(Platform::String^ file_prefix, Platform::String^ rootdir)
	: BitmapBooleanlet(file_prefix, 0.0F, 0.0F, rootdir) {}

BitmapBooleanlet::BitmapBooleanlet(Platform::String^ file_prefix, float width, float height, Platform::String^ rootdir)
	: BitmapBooleanlet(file_prefix + "_true", file_prefix + "_false", width, height, rootdir) {}

BitmapBooleanlet::BitmapBooleanlet(Platform::String^ t_file, Platform::String^ f_file, float width, float height, Platform::String^ rootdir) {
	this->window.Width = width;
	this->window.Height = height;
	this->ms_appx_tmp = ms_appx_path(t_file, rootdir, ".png");
	this->ms_appx_fmp = ms_appx_path(f_file, rootdir, ".png");
}

BitmapBooleanlet::~BitmapBooleanlet() {
	int uuids[] = { this->ms_appx_tmp->GetHashCode(), this->ms_appx_fmp->GetHashCode() };
	CanvasBitmap^ bmps[] = { this->graph_tmp, this->graph_fmp };

	for (int i = 0; i < 2; i++) {
		auto t = lazy_tasks.find(uuids[i]);

		if (t != lazy_tasks.end()) {
			if (bmps[i] == nullptr) {
				t->second.cancel();
			}

			lazy_tasks.erase(t);
		}
	}
}

void BitmapBooleanlet::construct() {
	int t_uuid = this->ms_appx_tmp->GetHashCode();
	int f_uuid = this->ms_appx_tmp->GetHashCode();
	cancellation_token_source tmp_task;
	cancellation_token_source fmp_task;

	lazy_tasks.insert(std::pair<int, cancellation_token_source>(t_uuid, tmp_task));
	lazy_tasks.insert(std::pair<int, cancellation_token_source>(f_uuid, fmp_task));
	this->load_async(this->ms_appx_tmp, tmp_task.get_token());
	this->load_async(this->ms_appx_fmp, fmp_task.get_token());
}

void BitmapBooleanlet::on_appx(Uri^ ms_appx, CanvasBitmap^ doc_bmp) {
	if (ms_appx == this->ms_appx_tmp) {
		this->graph_tmp = doc_bmp;
	} else {
		this->graph_fmp = doc_bmp;
	}

	// NOTE: The client application should guarantee that source bitmaps have the same size
	if (this->window.Width <= 0.0F) {
		if (this->window.Height <= 0.0F) {
			this->window.Height = doc_bmp->Size.Height;
		}
		this->window.Width = doc_bmp->Size.Width * (this->window.Height / doc_bmp->Size.Height);
	} else if (this->window.Height <= 0.0F) {
		this->window.Height = doc_bmp->Size.Height * (this->window.Width / doc_bmp->Size.Width);
	}
}

bool BitmapBooleanlet::ready() {
	if (this->get_scale()) {
		return (this->graph_tmp != nullptr);
	} else {
		return (this->graph_fmp != nullptr);
	}
}

void BitmapBooleanlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->window.Width, h, this->window.Height);
}

void BitmapBooleanlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	auto target = (this->get_scale() ? this->graph_tmp : this->graph_fmp);
	this->window.X = x;
	this->window.Y = y;

	ds->DrawImage(target, this->window);
}

void BitmapBooleanlet::draw_progress(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Uri^ target = (this->get_scale() ? this->ms_appx_tmp : this->ms_appx_fmp);
	Platform::String^ hint = file_name_from_path(target);

	draw_invalid_bitmap(hint, ds, x, y, this->window.Width, this->window.Height);
}
