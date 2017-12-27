#include <ppltasks.h>

#include "snip/misc.hpp"
#include "snip.hpp"
#include "shape.hpp"
#include "rsyslog.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::UI;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Geometry;

Rect snip_enclosing_box(Snip* snip, float x, float y, float3x2 transform) {
    float width, height;

    snip->fill_extent(x, y, &width, &height);
    
    return rectangle(x, y, width, height)->ComputeBounds(transform);
}

void snip_save(Snip* snip, Platform::String^ path, float dpi) {
	CanvasDevice^ shared_dc = CanvasDevice::GetSharedDevice();
	float width, height;

	snip->fill_extent(0.0F, 0.0F, &width, &height);

	CanvasRenderTarget^ bmp = ref new CanvasRenderTarget(shared_dc, width, height, dpi);
	CanvasDrawingSession^ ds = bmp->CreateDrawingSession();

	ds->Clear(ColorHelper::FromArgb(0, 255, 255, 255));
	snip->draw(ds, 0.0F, 0.0F, width, height);
	create_task(bmp->SaveAsync(path, CanvasBitmapFileFormat::Auto, 1.0F)).then([=](task<void> saving) {
		try {
			saving.get();
		} catch (Platform::Exception^ e) {
			rsyslog("failed to save snip as bitmap:" + e->Message);
		}
	});
}
