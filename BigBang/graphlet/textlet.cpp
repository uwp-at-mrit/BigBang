#include "graphlet/textlet.hpp"

#include "planet.hpp"
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

static inline float aligned_y(TextExtent& te, float by) {
	return by - (te.height - te.bspace);
}


/*************************************************************************************************/
void Textlet::set_color(ICanvasBrush^ color) {
	this->text_color = color;
	this->notify_updated();
}

void Textlet::set_color(unsigned int color_hex, double alpha) {
	this->set_color(Colours::make(color_hex, alpha));
}

void Textlet::set_font(CanvasTextFormat^ font, GraphletAnchor anchor) {
	this->moor(anchor);

	this->text_font = font;
	this->set_text(this->raw);
	this->on_font_changed();
	
	this->notify_updated();
}

void Textlet::set_text(Platform::String^ content, GraphletAnchor anchor) {
	this->raw = content;

	this->moor(anchor);

	if (this->text_font == nullptr) {
		this->set_font(default_text_font);
	} else if (this->raw == nullptr) {
		this->text_layout = nullptr;
	} else {
		this->text_layout = make_text_layout(this->raw, this->text_font);
	}

	this->notify_updated();
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
Dimensionlet::Dimensionlet(Platform::String^ unit, CanvasTextFormat^ nfont, CanvasTextFormat^ lfont, ICanvasBrush^ color)
	: Dimensionlet(unit, "", "", nfont, lfont, color, color) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, Platform::String^ label, CanvasTextFormat^ font, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: Dimensionlet(unit, label, "", font, font, ncolor, lcolor) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, Platform::String^ label, Platform::String^ subscript
	, CanvasTextFormat^ nfont, CanvasTextFormat^ lfont, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: num_color(ncolor) {
	auto num_font = ((nfont == nullptr) ? make_bold_text_format() : nfont);
	auto label_font = ((lfont == nullptr) ? num_font : lfont);

	this->set_color((lcolor == nullptr) ? ncolor : lcolor);
	this->set_font(num_font);
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

void Dimensionlet::construct() {
	this->set_value(0.0F, true);
}

void Dimensionlet::fill_extent(float x, float y, float* w, float* h) {
	if (w != nullptr) {
		(*w) = this->num_box.width + this->unit_box.width;

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

void Dimensionlet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	TextExtent label_box;
	float tspace, bspace;

	this->fill_vmetrics(&label_box, &tspace, &bspace);

	SET_VALUES(l, label_box.lspace, r, this->unit_box.rspace);
	SET_VALUES(t, tspace, b, bspace);
}

void Dimensionlet::on_value_changed(float value) {
	Platform::String^ s = value.ToString();
	
	if (this->text_layout != nullptr) {
		s = " " + s;
	}
		
	this->num_layout = make_text_layout(s, this->text_font);
	this->num_box = get_text_extent(this->num_layout);
}

void Dimensionlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	TextExtent label_box;
	float tspace, bspace, height, by;
	float lx = x;

	if (this->text_color == nullptr) {
		this->set_color();
	}

	if (this->num_color == nullptr) {
		this->num_color = this->text_color;
	}

	this->fill_vmetrics(&label_box, &tspace, &bspace, &height);
	by = y + height - bspace;

	if (this->text_layout != nullptr) {
		ds->DrawTextLayout(this->text_layout, x, aligned_y(label_box, by), this->text_color);
		lx += label_box.width;
	}

	ds->DrawTextLayout(this->num_layout, lx, aligned_y(this->num_box, by), this->num_color);
	ds->DrawTextLayout(this->unit_layout, lx + this->num_box.width, aligned_y(this->unit_box, by), this->text_color);
}

void Dimensionlet::fill_vmetrics(TextExtent* label_box, float* tspace, float* bspace, float* height) {
	(*label_box) = ((this->text_layout == nullptr) ? this->num_box : get_text_extent(this->text_layout));
	(*tspace) = fmin(label_box->tspace, fmin(this->num_box.tspace, this->unit_box.tspace));
	(*bspace) = fmin(label_box->bspace, fmin(this->num_box.bspace, this->unit_box.bspace));

	if (height != nullptr) {
		float hsink = this->num_box.height - this->num_box.tspace - this->num_box.bspace;
		float huink = this->unit_box.height - this->unit_box.tspace - this->unit_box.bspace;
		float ink_height = fmax(hsink, huink);

		if (this->text_layout != nullptr) {
			ink_height = fmax(ink_height, label_box->height - label_box->tspace - label_box->bspace);
		}

		(*height) = (*tspace) + ink_height + (*bspace);
	}
}
