#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

static CanvasDevice^ shared_ds = CanvasDevice::GetSharedDevice();

CanvasTextLayout^ make_text_layout(Platform::String^ para, CanvasTextFormat^ font) {
    return ref new CanvasTextLayout(shared_ds, para, font, 0.0F, 0.0F);
}

CanvasTextLayout^ make_vertical_layout(Platform::String^ para, CanvasTextFormat^ font, float spacing, CanvasHorizontalAlignment align) {
    auto layout = make_text_layout(para, font);

    layout->WordWrapping = CanvasWordWrapping::WholeWord;
    layout->HorizontalAlignment = align;

    if (spacing > 0.0F) {
        layout->LineSpacing = spacing;
    } else if (spacing < 0.0F) {
        layout->LineSpacing = -spacing;
        layout->LineSpacingMode = CanvasLineSpacingMode::Proportional;
    }

    return layout;
}

CanvasTextFormat^ make_text_format(float size, CanvasWordWrapping wrapping, CanvasHorizontalAlignment align) {
    return make_text_format(nullptr, size, wrapping, align);
}

CanvasTextFormat^ make_text_format(Platform::String^ face, float size, CanvasWordWrapping wrapping, CanvasHorizontalAlignment align) {
	auto font_config = ref new CanvasTextFormat();

	if (face != nullptr) {
		font_config->FontFamily = face;
	}

	font_config->WordWrapping = wrapping;
	font_config->HorizontalAlignment = align;
	font_config->FontSize = size;


	return font_config;
}

TextExtent get_text_extent(Platform::String^ message, CanvasTextFormat^ font, bool trim) {
    return get_text_extent(shared_ds, message, font, trim);
}

TextExtent get_text_extent(ICanvasResourceCreator^ ds, Platform::String^ message, CanvasTextFormat^ font, bool trim) {
    auto layout = ref new CanvasTextLayout(ds, message, font, 0.0F, 0.0F);
    Rect logical = trim ? layout->LayoutBounds : layout->LayoutBoundsIncludingTrailingWhitespace;
    Rect ink = layout->DrawBounds;
    float top = ink.Y - logical.Y;
    float bottom = logical.Height - ink.Height - top;
    float left = ink.X - logical.X;
    float right = logical.Width - ink.Width - left;

    return { logical.Width, logical.Height, top, right, bottom, left };
}
