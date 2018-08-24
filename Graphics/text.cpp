#include <cmath>

#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::UI::Text;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

CanvasTextLayout^ WarGrey::SCADA::make_text_layout(Platform::String^ para, CanvasTextFormat^ font) {
    return ref new CanvasTextLayout(CanvasDevice::GetSharedDevice(), para, font, 0.0F, 0.0F);
}

CanvasTextLayout^ WarGrey::SCADA::make_vertical_layout(Platform::String^ para, CanvasTextFormat^ font, float spacing, CanvasHorizontalAlignment align) {
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

CanvasTextFormat^ WarGrey::SCADA::make_text_format(CanvasTextFormat^ src, float size) {
	CanvasTextFormat^ font = make_text_format(src->FontFamily, size, src->WordWrapping, src->HorizontalAlignment);

	font->FontWeight = src->FontWeight;
	font->FontStretch = src->FontStretch;
	font->FontStyle = src->FontStyle;

	return font;
}

CanvasTextFormat^ WarGrey::SCADA::make_text_format(CanvasTextFormat^ font, double size) {
	return make_text_format(font, float(std::round(font->FontSize * size)));
}

CanvasTextFormat^ WarGrey::SCADA::make_text_format(float size, CanvasWordWrapping wrapping, CanvasHorizontalAlignment align) {
    return make_text_format(nullptr, size, wrapping, align);
}

CanvasTextFormat^ WarGrey::SCADA::make_text_format(Platform::String^ face, float size, CanvasWordWrapping wrapping, CanvasHorizontalAlignment align) {
	auto font_config = ref new CanvasTextFormat();

	if (face != nullptr) {
		font_config->FontFamily = face;
	}

	font_config->WordWrapping = wrapping;
	font_config->HorizontalAlignment = align;
	font_config->FontSize = size;

	return font_config;
}

CanvasTextFormat^ WarGrey::SCADA::make_bold_text_format(float size, CanvasWordWrapping wrapping, CanvasHorizontalAlignment align) {
	auto font = make_text_format(nullptr, size, wrapping, align);
	
	font->FontWeight = FontWeights::Bold;

	return font;
}

CanvasTextFormat^ WarGrey::SCADA::make_bold_text_format(Platform::String^ face, float size, CanvasWordWrapping wrapping, CanvasHorizontalAlignment align) {
	auto font = make_text_format(face, size, wrapping, align);

	font->FontWeight = FontWeights::Bold;

	return font;
}


CanvasTextFormat^ WarGrey::SCADA::make_italic_text_format(float size, CanvasWordWrapping wrapping, CanvasHorizontalAlignment align) {
	auto font = make_text_format(nullptr, size, wrapping, align);

	font->FontStyle = FontStyle::Italic;

	return font;
}

CanvasTextFormat^ WarGrey::SCADA::make_italic_text_format(Platform::String^ face, float size, CanvasWordWrapping wrapping, CanvasHorizontalAlignment align) {
	auto font = make_text_format(face, size, wrapping, align);

	font->FontStyle = FontStyle::Italic;

	return font;
}

TextExtent WarGrey::SCADA::get_text_extent(Platform::String^ message, CanvasTextFormat^ font, bool trim) {
    return get_text_extent(CanvasDevice::GetSharedDevice(), message, font, trim);
}

TextExtent WarGrey::SCADA::get_text_extent(ICanvasResourceCreator^ ds, Platform::String^ message, CanvasTextFormat^ font, bool trim) {
    return get_text_extent(ref new CanvasTextLayout(ds, message, font, 0.0F, 0.0F), trim);    
}

TextExtent WarGrey::SCADA::get_text_extent(CanvasTextLayout^ layout, bool trim) {
	Rect logical = (trim ? layout->LayoutBounds : layout->LayoutBoundsIncludingTrailingWhitespace);
	Rect ink = layout->DrawBounds;
	TextExtent te;

	te.width = logical.Width;
	te.height = logical.Height;
	te.tspace = ink.Y - logical.Y;
	te.bspace = logical.Height - ink.Height - te.tspace;
	te.lspace = ink.X - logical.X;
	te.rspace = logical.Width - ink.Width - te.lspace;

	return te;
}
