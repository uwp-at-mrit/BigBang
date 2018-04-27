#include "graphlet/textlet.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "tongue.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Text;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasTextFormat^ default_text_font = make_bold_text_format();
/*************************************************************************************************/
void Textlet::set_color(ICanvasBrush^ color) {
	this->text_color = color;
}

void Textlet::set_color(unsigned int color_hex, double alpha) {
	this->set_color(Colours::make(color_hex, alpha));
}

void Textlet::set_font(CanvasTextFormat^ font) {
	this->text_font = font;
	this->set_text(this->raw);
	this->on_font_change();
}

void Textlet::set_text(Platform::String^ content) {
	this->raw = content;

	if (this->text_font == nullptr) {
		this->set_font(default_text_font);
	} else if (this->raw == nullptr) {
		this->text_layout = nullptr;
	} else {
		this->text_layout = make_text_layout(this->raw, this->text_font);
	}
}

void Textlet::set_text(const wchar_t *fmt, ...) {
	VSWPRINT(content, fmt);
	this->set_text(content);
}

void Textlet::set_layout_font_size(int char_idx, int char_count, float size) {
	if (this->text_layout != nullptr) {
		this->text_layout->SetFontSize(char_idx, char_count, size);
	}
}

void Textlet::set_layout_font_style(int char_idx, int char_count, Windows::UI::Text::FontStyle style) {
	if (this->text_layout != nullptr) {
		this->text_layout->SetFontStyle(char_idx, char_count, style);
	}
}

void Textlet::fill_extent(float x, float y, float* w, float* h) {
	if (this->text_layout != nullptr) {
		auto box = this->text_layout->LayoutBounds;

		SET_VALUES(w, box.Width, h, box.Height);
	} else {
		SET_BOXES(w, h, 0.0F);
	}
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

Labellet::Labellet(Platform::String^ content, unsigned int color_hex, double alpha)
	: Labellet(content, nullptr, color_hex, alpha) {
}

Labellet::Labellet(Platform::String^ content, CanvasTextFormat^ font, ICanvasBrush^ color) {
	if (font != nullptr) {
		this->set_font(font);
	}

	if (color != nullptr) {
		this->set_color(color);
	}

	this->set_text(content);
}

Labellet::Labellet(Platform::String^ content, CanvasTextFormat^ font, unsigned int color_hex, double alpha) {
	if (font != nullptr) {
		this->set_font(font);
	}

	this->set_color(color_hex, alpha);
	this->set_text(content);
}

/*************************************************************************************************/
ScaleTextlet::ScaleTextlet(Platform::String^ unit, Platform::String^ label, Platform::String^ subscript, Colour^ lcolor, Colour^ scolor)
	: scale_color(scolor) {
	this->set_color(lcolor);
	this->set_font(make_bold_text_format());
	this->unit_layout = make_text_layout(speak(unit), this->text_font);

	if (label != nullptr) {
		Platform::String^ symbol = speak(label);
		Platform::String^ suffix = ":";

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
}

void ScaleTextlet::construct() {
	this->set_scale(0.0F, true);
}

void ScaleTextlet::fill_extent(float x, float y, float* w, float* h) {
	Textlet::fill_extent(x, y, w, h);

	if (w != nullptr) {
		(*w) += (this->scale_layout->LayoutBounds.Width + this->unit_layout->LayoutBounds.Width);
	}

	if (h != nullptr) {
		(*h) = fmax((*h), fmax(this->scale_layout->LayoutBounds.Height, this->unit_layout->LayoutBounds.Height));
	}
}

void ScaleTextlet::on_scale_change(float scale) {
	Platform::String^ s = scale.ToString();
	
	if (this->text_layout != nullptr) {
		s = " " + s;
	}
		
	this->scale_layout = make_text_layout(s, this->text_font);
}

void ScaleTextlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float start_x = x;

	if (this->text_layout != nullptr) {
		ds->DrawTextLayout(this->text_layout, x, y, this->text_color);
		start_x += this->text_layout->LayoutBounds.Width;
	}

	ds->DrawTextLayout(this->scale_layout, start_x, y, this->scale_color);
	ds->DrawTextLayout(this->unit_layout, start_x + this->scale_layout->LayoutBounds.Width, y, this->text_color);
}
