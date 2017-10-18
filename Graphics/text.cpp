#include "text.hpp"

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

static CanvasDrawingSession^ shared_ds;

CanvasDrawingSession^ make_shared_drawing_session() {
    if (shared_ds == nullptr) {
        auto sharedDevice = CanvasDevice::GetSharedDevice();
        auto target = ref new CanvasRenderTarget(sharedDevice, 1.0F, 1.0F, 96.0F);
        shared_ds = target->CreateDrawingSession();
    }

    return shared_ds;
}

CanvasTextFormat^ make_text_format(float size) {
    auto font_config = ref new CanvasTextFormat();

    font_config->WordWrapping = CanvasWordWrapping::NoWrap;
    font_config->FontSize = size;

    return font_config;
}

TextExtent get_text_extent(Platform::String^ message, CanvasTextFormat^ label_font) {
    return get_text_extent(make_shared_drawing_session(), message, label_font);
}

TextExtent get_text_extent(CanvasDrawingSession^ ds, Platform::String^ message, CanvasTextFormat^ label_font) {
    auto layout = ref new CanvasTextLayout(ds, message, label_font, 0.0F, 0.0F);
    Rect logical = layout->LayoutBounds;
    Rect ink = layout->DrawBounds;
    float top = ink.Y - logical.Y;
    float bottom = logical.Height - ink.Height - top;
    float left = ink.X - logical.X;
    float right = logical.Width - ink.Width - left;

    return { logical.Width, logical.Height, top, right, bottom, left };
}
