#include <ppltasks.h>

#include "snip/misc.hpp"
#include "snip.hpp"
#include "shape.hpp"
#include "syslog.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

Rect snip_enclosing_box(ISnip* snip, float x, float y, float3x2 transform) {
    float width, height;

    snip->fill_extent(x, y, &width, &height);
    
    return rectangle(x, y, width, height)->ComputeBounds(transform);
}

CanvasRenderTarget^ snip_snapshot(ISnip* snip, float dpi) {
	CanvasDevice^ shared_dc = CanvasDevice::GetSharedDevice();
	float width, height;

	snip->fill_extent(0.0F, 0.0F, &width, &height);

	{ // WARNING: there is no synchronous mechanism for snip.
		CanvasRenderTarget^ snapshot = ref new CanvasRenderTarget(shared_dc, width, height, dpi);
		CanvasDrawingSession^ ds = snapshot->CreateDrawingSession();

		ds->Clear(ColorHelper::FromArgb(0, 255, 255, 255));
		snip->draw(ds, 0.0F, 0.0F, width, height);

		return snapshot;
	}
}

void snip_save(ISnip* snip, Platform::String^ path, float dpi) {
	CanvasRenderTarget^ snapshot = snip_snapshot(snip, dpi);
	
	create_task(snapshot->SaveAsync(path, CanvasBitmapFileFormat::Auto, 1.0F)).then([=](task<void> saving) {
		try {
			saving.get();
		} catch (Platform::Exception^ e) {
			syslog(Log::Alert, "failed to save snip as bitmap:" + e->Message);
		}
	});
}
