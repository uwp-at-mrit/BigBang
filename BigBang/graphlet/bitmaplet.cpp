#include <ppltasks.h>
#include <map>

#include "graphlet/bitmaplet.hpp"

#include "colorspace.hpp"
#include "path.hpp"
#include "draw.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::UI;
using namespace Windows::Foundation;

using namespace Windows::Storage;
using namespace Windows::Storage::Streams;

using namespace Microsoft::Graphics::Canvas;

static std::map<int, cancellation_token_source> lazy_tasks;

/*************************************************************************************************/
Bitmaplet::Bitmaplet(Platform::String^ file, Platform::String^ rootdir) : Bitmaplet(file, 0.0F, 0.0F, rootdir) {}

Bitmaplet::Bitmaplet(Platform::String^ file, float width, float height, Platform::String^ rootdir) {
	this->viewport.Width = width;
	this->viewport.Height = height;
	this->ms_appx_bmp = ms_appx_path(file, rootdir, ".bmp");
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
	cancellation_token bmp_token = bmp_task.get_token();

	lazy_tasks.insert(std::pair<int, cancellation_token_source>(uuid, bmp_task));

	create_task(StorageFile::GetFileFromApplicationUriAsync(this->ms_appx_bmp), bmp_token).then([=](task<StorageFile^> svg) {
		this->get_logger()->log_message(Log::Debug,
			L"Found the graphlet source: %s",
			this->ms_appx_bmp->ToString()->Data());

		return create_task(svg.get()->OpenAsync(FileAccessMode::Read, StorageOpenOptions::AllowOnlyReaders), bmp_token);
	}).then([=](task<IRandomAccessStream^> bmp) {
		return create_task(CanvasBitmap::LoadAsync(CanvasDevice::GetSharedDevice(), bmp.get()), bmp_token);
	}).then([=](task<CanvasBitmap^> doc_bmp) {
		try {
			this->graph_bmp = doc_bmp.get();
			
			if (this->viewport.Width == 0.0F) {
				this->viewport.Width = this->graph_bmp->Size.Width;
			}

			if (this->viewport.Height == 0.0F) {
				this->viewport.Height = this->graph_bmp->Size.Height;
			}
		} catch (Platform::Exception^ e) {
			this->get_logger()->log_message(Log::Error,
				L"Failed to load %s: %s",
				this->ms_appx_bmp->ToString()->Data(),
				e->Message->Data());
		} catch (task_canceled&) {
			this->get_logger()->log_message(Log::Debug,
				L"Cancelled loading %s",
				this->ms_appx_bmp->ToString()->Data());
		}
	});
}

void Bitmaplet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->viewport.Width, h, this->viewport.Height);
}

void Bitmaplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->graph_bmp != nullptr) {
		this->viewport.X = x;
		this->viewport.Y = y;

		ds->DrawImage(this->graph_bmp, this->viewport);
	} else {
		this->draw_on_error(ds, x, y, Width, Height);
	}
}

void Bitmaplet::draw_on_error(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Platform::String^ hint = file_name_from_path(this->ms_appx_bmp);

	draw_invalid_bitmap(hint, ds, x, y, this->viewport.Width, this->viewport.Height);
}
