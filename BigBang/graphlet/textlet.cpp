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
using namespace Microsoft::Graphics::Canvas::Brushes;

void Textlet::set_color(CanvasSolidColorBrush^ color) {
	this->text_color = color;
}

void Textlet::set_font(CanvasTextFormat^ font) {
	this->text_font = font;
	this->set_text(this->raw);
}

void Textlet::set_text(Platform::String^ content) {
	if (this->text_font == nullptr) {
		this->text_font = make_text_format();
		this->text_font->FontWeight = FontWeights::Bold;
	}

	this->raw = content;
	this->text_layout = make_text_layout(content, this->text_font);
}

void Textlet::set_layout_font_size(int char_idx, int char_count, float size) {
	this->text_layout->SetFontSize(char_idx, char_count, size);
}

void Textlet::set_layout_font_style(int char_idx, int char_count, Windows::UI::Text::FontStyle style) {
	this->text_layout->SetFontStyle(char_idx, char_count, style);
}

void Textlet::fill_extent(float x, float y, float* w, float* h) {
	auto box = this->text_layout->LayoutBounds;

	SET_VALUES(w, box.Width, h, box.Height);
}

void Textlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->text_layout != nullptr) {
		if (this->text_color == nullptr) {
			this->set_color();
		}

		ds->DrawTextLayout(this->text_layout, x, y, this->text_color);
	}
}

/*************************************************************************************************/
Labellet::Labellet(const wchar_t *fmt, ...) {
	VSWPRINT(label, fmt);
    this->set_text(label);
}

Labellet::Labellet(Platform::String^ content) {
	this->set_text(content);
}

/*************************************************************************************************/
ScaleTextlet::ScaleTextlet(Platform::String^ unit, Platform::String^ label, Platform::String^ subscript, Colour^ lcolor, Colour^ scolor)
	: scale_color(scolor) {
	this->set_color(lcolor);
	this->set_font(make_text_format("Courier New"));
	this->unit_layout = make_text_layout(speak(unit), this->text_font);

	if (label != nullptr) {
		Platform::String^ symbol = speak(label);
		Platform::String^ suffix = " =";

		if (subscript == nullptr) {
			this->set_text(symbol + suffix);
		} else {
			Platform::String^ subsymbol = speak(subscript);
			float subsize = this->text_font->FontSize * 0.618F;
			unsigned int symcount = symbol->Length();
			unsigned int subcount = subsymbol->Length();

			this->set_text(symbol + subsymbol + suffix);
			this->set_layout_font_size(symcount, subcount, subsize);
			this->set_layout_font_style(symcount, subcount, FontStyle::Italic);
		}
	}

	this->set_scale(0.0F);
}

void ScaleTextlet::on_scale_change(float scale) {
	this->scale_layout = make_text_layout(" " + scale.ToString(), this->text_font);
}

void ScaleTextlet::fill_extent(float x, float y, float* w, float* h) {
	float width = this->scale_layout->LayoutBounds.Width + this->unit_layout->LayoutBounds.Width;
	float height = this->unit_layout->LayoutBounds.Height;

	if (this->text_layout != nullptr) {
		width += this->text_layout->LayoutBounds.Width;
	}

	SET_VALUES(w, width, h, height);
}

void ScaleTextlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float start_x = x;
	float swidth = this->scale_layout->LayoutBounds.Width;

	if (this->text_layout != nullptr) {
		ds->DrawTextLayout(this->text_layout, x, y, this->text_color);
		start_x += this->text_layout->LayoutBounds.Width;
	}

	ds->DrawTextLayout(this->scale_layout, start_x, y, this->scale_color);
	ds->DrawTextLayout(this->unit_layout, start_x + swidth, y, this->text_color);
}
