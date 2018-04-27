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
