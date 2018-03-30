#include "graphlet/textlet.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "tongue.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::UI::Text;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

Labellet::Labellet(const wchar_t *fmt, ...) {
	VSWPRINT(label, fmt);
	this->set_color();
    this->set_text(label);
}

Labellet::Labellet(Colour^ color, const wchar_t *fmt, ...) {
	VSWPRINT(label, fmt);
	this->set_color(color);
	this->set_text(label);
}

Labellet::Labellet(CanvasTextFormat^ font, const wchar_t *fmt, ...) {
	VSWPRINT(label, fmt);
	this->set_font(font);
	this->set_color();
	this->set_text(label);
}

Labellet::Labellet(CanvasTextFormat^ font, Colour^ color, const wchar_t *fmt, ...) {
	VSWPRINT(label, fmt);
	this->set_font(font);
	this->set_color(color);
	this->set_text(label);
}

Labellet::Labellet(Platform::String^ content) {
	this->set_color();
	this->set_text(content);
}

Labellet::Labellet(Colour^ color, Platform::String^ content) {
	this->set_color(color);
    this->set_text(content);
}

Labellet::Labellet(CanvasTextFormat^ font, Platform::String^ content) {
	this->set_font(font);
	this->set_color();
	this->set_text(content);
}

Labellet::Labellet(CanvasTextFormat^ font, Colour^ color, Platform::String^ content) {
	this->set_font(font);
	this->set_color(color);
	this->set_text(content);
}

void Labellet::set_text(Platform::String^ content) {
    if (this->label_font == nullptr) {
        this->label_font = make_text_format();
		this->label_font->FontWeight = FontWeights::Bold;
    }

	this->content = make_text_layout(content, this->label_font);
}

void Labellet::set_color(Colour^ color) {
	this->label_color = color;
}

void Labellet::set_font(CanvasTextFormat^ font) {
	this->label_font = font;
}

void Labellet::fill_extent(float x, float y, float* w, float* h) {
	auto box = this->content->LayoutBounds;

    SET_VALUES(w, box.Width, h, box.Height);
}

void Labellet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
    ds->DrawTextLayout(this->content, x, y, this->label_color);
}

/*************************************************************************************************/
Scalelet::Scalelet(Platform::String^ unit, Platform::String^ label, Platform::String^ subscript, Colour^ lcolor, Colour^ scolor)
	: label_color(lcolor), scale_color(scolor) {
	this->scale_font = make_text_format("Courier New");
	this->unit = make_text_layout(speak(unit), this->scale_font);

	if (label != nullptr) {
		Platform::String^ symbol = speak(label);
		Platform::String^ suffix = " =";

		if (subscript == nullptr) {
			this->label = make_text_layout(symbol + suffix, this->scale_font);
		} else {
			Platform::String^ subsymbol = speak(subscript);
			float subsize = this->scale_font->FontSize * 0.618F;
			unsigned int symcount = symbol->Length();
			unsigned int subcount = subsymbol->Length();

			this->label = make_text_layout(symbol + subsymbol + suffix, this->scale_font);
			this->label->SetFontSize(symcount, subcount, subsize);
			this->label->SetFontStyle(symcount, subcount, FontStyle::Italic);
		}
	}

	this->set_scale(0.0F);
}

void Scalelet::set_scale(float value) {
	this->scale = make_text_layout(" " + value.ToString(), this->scale_font);
}

void Scalelet::fill_extent(float x, float y, float* w, float* h) {
	float width = this->scale->LayoutBounds.Width + this->unit->LayoutBounds.Width;
	float height = this->unit->LayoutBounds.Height;

	if (this->label != nullptr) {
		width += this->label->LayoutBounds.Width;
	}

	SET_VALUES(w, width, h, height);
}

void Scalelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float start_x = x;
	float swidth = this->scale->LayoutBounds.Width;

	if (this->label != nullptr) {
		ds->DrawTextLayout(this->label, x, y, this->label_color);
		start_x += this->label->LayoutBounds.Width;
	}

	ds->DrawTextLayout(this->scale, start_x, y, this->scale_color);
	ds->DrawTextLayout(this->unit, start_x + swidth, y, this->label_color);
}
