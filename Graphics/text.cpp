#include <cmath>

#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::UI::Text;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

CanvasTextLayout^ make_text_layout(Platform::String^ para, CanvasTextFormat^ font) {
    return ref new CanvasTextLayout(CanvasDevice::GetSharedDevice(), para, font, 0.0F, 0.0F);
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

CanvasTextFormat^ make_text_format(CanvasTextFormat^ src, float size) {
	CanvasTextFormat^ font = make_text_format(src->FontFamily, size, src->WordWrapping, src->HorizontalAlignment);

	font->FontWeight = src->FontWeight;
	font->FontStretch = src->FontStretch;
	font->FontStyle = src->FontStyle;

	return font;
}

CanvasTextFormat^ make_text_format(CanvasTextFormat^ font, double size) {
	return make_text_format(font, float(std::round(font->FontSize * size)));
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

CanvasTextFormat^ make_bold_text_format(float size, CanvasWordWrapping wrapping, CanvasHorizontalAlignment align) {
	auto font = make_text_format(nullptr, size, wrapping, align);
	
	font->FontWeight = FontWeights::Bold;

	return font;
}

CanvasTextFormat^ make_bold_text_format(Platform::String^ face, float size, CanvasWordWrapping wrapping, CanvasHorizontalAlignment align) {
	auto font = make_text_format(face, size, wrapping, align);

	font->FontWeight = FontWeights::Bold;

	return font;
}

TextExtent get_text_extent(Platform::String^ message, CanvasTextFormat^ font, bool trim) {
    return get_text_extent(CanvasDevice::GetSharedDevice(), message, font, trim);
}

TextExtent get_text_extent(ICanvasResourceCreator^ ds, Platform::String^ message, CanvasTextFormat^ font, bool trim) {
    return get_text_extent(ref new CanvasTextLayout(ds, message, font, 0.0F, 0.0F), trim);    
}

TextExtent get_text_extent(CanvasTextLayout^ layout, bool trim) {
	Rect logical = trim ? layout->LayoutBounds : layout->LayoutBoundsIncludingTrailingWhitespace;
	Rect ink = layout->DrawBounds;
	float top = ink.Y - logical.Y;
	float bottom = logical.Height - ink.Height - top;
	float left = ink.X - logical.X;
	float right = logical.Width - ink.Width - left;

	return { logical.Width, logical.Height, top, right, bottom, left };
}
