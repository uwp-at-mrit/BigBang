#include "graphlet/textlet.hpp"

#include "paint.hpp"
#include "tongue.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::UI::Text;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasTextFormat^ default_text_font = make_bold_text_format();

static inline float layout_y(TextExtent& te, float by) {
	return by - (te.height - te.bspace);
}


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

void Textlet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	if (this->text_layout != nullptr) {
		TextExtent te = get_text_extent(this->text_layout);

		SET_VALUES(t, te.tspace, b, te.bspace);
		SET_VALUES(l, te.lspace, r, te.rspace);
	} else {
		IGraphlet::fill_margin(x, y, t, r, b, l);
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
ScaleTextlet::ScaleTextlet(Platform::String^ unit, CanvasTextFormat^ sfont, CanvasTextFormat^ lfont, ICanvasBrush^ color)
	: ScaleTextlet(unit, "", "", sfont, lfont, color, color) {}
	
ScaleTextlet::ScaleTextlet(Platform::String^ unit, Platform::String^ label, Platform::String^ subscript
	, CanvasTextFormat^ sfont, CanvasTextFormat^ lfont, ICanvasBrush^ scolor, ICanvasBrush^ lcolor)
	: scale_color(scolor) {
	auto scale_font = ((sfont == nullptr) ? make_bold_text_format() : sfont);
	auto label_font = ((lfont == nullptr) ? scale_font : lfont);

	this->set_color((lcolor == nullptr) ? scolor : lcolor);
	this->set_font(scale_font);
	this->unit_layout = make_text_layout(speak(unit), label_font);
	this->unit_box = get_text_extent(this->unit_layout);

	if (label != nullptr) {
		Platform::String^ symbol = speak(label);
		Platform::String^ suffix = ":";

		if (subscript == nullptr) {
			this->set_text(symbol + suffix);
		} else {
			Platform::String^ subsymbol = speak(subscript);
			float subsize = label_font->FontSize * 0.618F;
			unsigned int symcount = symbol->Length();
			unsigned int subcount = subsymbol->Length();

			this->set_text(symbol + subsymbol + suffix);
			this->set_layout_font_size(symcount, subcount, subsize);
			this->set_layout_font_style(symcount, subcount, FontStyle::Italic);
		}
	}
}

void ScaleTextlet::construct() {
	this->set_value(0.0F, true);
}

void ScaleTextlet::fill_extent(float x, float y, float* w, float* h) {
	if (w != nullptr) {
		(*w) = this->scale_box.width + this->unit_box.width;

		if (this->text_layout != nullptr) {
			(*w) += this->text_layout->LayoutBounds.Width;
		}
	}

	if (h != nullptr) {
		TextExtent label_box;
		float tspace, bspace;
		
		this->fill_vmetrics(&label_box, &tspace, &bspace, h);
	}
}

void ScaleTextlet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	TextExtent label_box;
	float tspace, bspace;

	this->fill_vmetrics(&label_box, &tspace, &bspace);

	SET_VALUES(l, label_box.lspace, r, this->unit_box.rspace);
	SET_VALUES(t, tspace, b, bspace);
}

void ScaleTextlet::on_value_change(float value) {
	Platform::String^ s = value.ToString();
	
	if (this->text_layout != nullptr) {
		s = " " + s;
	}
		
	this->scale_layout = make_text_layout(s, this->text_font);
	this->scale_box = get_text_extent(this->scale_layout);
}

void ScaleTextlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	TextExtent label_box;
	float tspace, bspace, height, by;
	float lx = x;

	if (this->text_color == nullptr) {
		this->set_color();
	}

	if (this->scale_color == nullptr) {
		this->scale_color = this->text_color;
	}

	this->fill_vmetrics(&label_box, &tspace, &bspace, &height);
	by = y + height - bspace;

	if (this->text_layout != nullptr) {
		ds->DrawTextLayout(this->text_layout, x, layout_y(label_box, by), this->text_color);
		lx += label_box.width;
	}

	ds->DrawTextLayout(this->scale_layout, lx, layout_y(this->scale_box, by), this->scale_color);
	ds->DrawTextLayout(this->unit_layout, lx + this->scale_box.width, layout_y(this->unit_box, by), this->text_color);
}

void ScaleTextlet::fill_vmetrics(TextExtent* label_box, float* tspace, float* bspace, float* height) {
	(*label_box) = ((this->text_layout == nullptr) ? this->scale_box : get_text_extent(this->text_layout));
	(*tspace) = fmin(label_box->tspace, fmin(this->scale_box.tspace, this->unit_box.tspace));
	(*bspace) = fmin(label_box->bspace, fmin(this->scale_box.bspace, this->unit_box.bspace));

	if (height != nullptr) {
		float hsink = this->scale_box.height - this->scale_box.tspace - this->scale_box.bspace;
		float huink = this->unit_box.height - this->unit_box.tspace - this->unit_box.bspace;
		float ink_height = fmax(hsink, huink);

		if (this->text_layout != nullptr) {
			ink_height = fmax(ink_height, label_box->height - label_box->tspace - label_box->bspace);
		}

		(*height) = (*tspace) + ink_height + (*bspace);
	}
}
