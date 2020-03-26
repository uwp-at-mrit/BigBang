#include <ppltasks.h>

#include "datum/box.hpp"

#include "sprite.hpp"
#include "syslog.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::GYDM;

using namespace Concurrency;

using namespace Windows::UI;
using namespace Microsoft::Graphics::Canvas;

void ISprite::fill_margin(float x, float y, float* top, float* right, float* bottom, float* left) {
	SET_BOXES(top, bottom, 0.0F);
	SET_BOXES(right, left, 0.0F);
}

CanvasRenderTarget^ ISprite::take_snapshot(float dpi) {
	CanvasDevice^ shared_dc = CanvasDevice::GetSharedDevice();
	float width, height;

	this->fill_extent(0.0F, 0.0F, &width, &height);

	{ // WARNING: there is no synchronous mechanism for graphlet.
		CanvasRenderTarget^ snapshot = ref new CanvasRenderTarget(shared_dc, width, height, dpi);
		CanvasDrawingSession^ ds = snapshot->CreateDrawingSession();

		ds->Clear(ColorHelper::FromArgb(0, 255, 255, 255));

		if (this->ready()) {
			this->draw(ds, 0.0F, 0.0F, width, height);
		} else {
			this->draw_progress(ds, 0.0F, 0.0F, width, height);
		}

		return snapshot;
	}
}

void ISprite::save(Platform::String^ path, float dpi) {
	CanvasRenderTarget^ snapshot = this->take_snapshot(dpi);

	create_task(snapshot->SaveAsync(path, CanvasBitmapFileFormat::Auto, 1.0F)).then([=](task<void> saving) {
		try {
			saving.get();

			this->get_logger()->log_message(Log::Notice, L"Sprite has been saved to %s", path->Data());
		} catch (Platform::Exception^ e) {
			syslog(Log::Alarm, L"failed to save sprite as bitmap: %s", e->Message->Data());
		}
	});
}
