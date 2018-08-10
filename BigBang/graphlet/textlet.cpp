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
static CanvasTextFormat^ default_math_font = make_bold_text_format("Cambria Math", 16.0F);

static inline Platform::String^ unit_speak(Platform::String^ unit) {
	bool exists;
	Platform::String^ dialect = speak(unit, "unit", &exists);

	return (exists ? dialect : speak(unit));
}

static inline Platform::String^ scalar(float value, bool leader) {
	Platform::String^ s = value.ToString();

	if (!leader) {
		s = " " + s;
	}

	return s;
}

static void fill_vmetrics(CanvasTextLayout^ layout, TextExtent& num_box, TextExtent& unit_box
	, TextExtent* label_box, float* tspace, float* bspace, float* height = nullptr) {
	(*label_box) = ((layout == nullptr) ? num_box : get_text_extent(layout));
	(*tspace) = fmin(label_box->tspace, fmin(num_box.tspace, unit_box.tspace));
	(*bspace) = fmin(label_box->bspace, fmin(num_box.bspace, unit_box.bspace));

	if (height != nullptr) {
		float link = label_box->height - label_box->tspace - unit_box.bspace;
		float nink = num_box.height - num_box.tspace - unit_box.bspace;
		float uink = unit_box.height - unit_box.tspace - unit_box.bspace;
		float ink_height = fmax(fmax(nink, uink), link);

		(*height) = (*tspace) + ink_height + (*bspace);
	}
}

void do_fill_extent(CanvasTextLayout^ layout, TextExtent& nbox, TextExtent& ubox, float x, float y, float* w, float* h) {
	if (w != nullptr) {
		(*w) = nbox.width + ubox.width;

		if (layout != nullptr) {
			(*w) += layout->LayoutBounds.Width;
		}
	}

	if (h != nullptr) {
		TextExtent label_box;
		float tspace, bspace;

		fill_vmetrics(layout, nbox, ubox, &label_box, &tspace, &bspace, h);
	}
}

static void do_fill_margin(CanvasTextLayout^ text_layout, TextExtent& nbox, TextExtent& ubox, float* t, float* r, float* b, float* l) {
	TextExtent label_box;
	float tspace, bspace;

	fill_vmetrics(text_layout, nbox, ubox, &label_box, &tspace, &bspace);

	SET_VALUES(l, label_box.lspace, r, ubox.rspace);
	SET_VALUES(t, tspace, b, bspace);
}

static void draw_metrics(CanvasDrawingSession^ ds, float x, float y, TextExtent& num_box, TextExtent& unit_box
	, CanvasTextLayout^ text_layout, CanvasTextLayout^ num_layout, CanvasTextLayout^ unit_layout
	, ICanvasBrush^ num_color, ICanvasBrush^ text_color) {
	TextExtent label_box;
	float tspace, bspace, height, base_y;
	float lx = x;

	fill_vmetrics(text_layout, num_box, unit_box, &label_box, &tspace, &bspace, &height);
	base_y = y + height;

	if (text_layout != nullptr) {
		ds->DrawTextLayout(text_layout, x, base_y - label_box.height, text_color);
		lx += label_box.width;
	}

	ds->DrawTextLayout(num_layout, lx, base_y - num_box.height, num_color);
	ds->DrawTextLayout(unit_layout, lx + num_box.width, base_y - unit_box.height, text_color);
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
	this->subscript_fontsize = font->FontSize * 0.618F;
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

void Textlet::set_subtext(Platform::String^ symbol, Platform::String^ subsymbol, Platform::String^ suffix) {
	unsigned int symcount = symbol->Length();
	unsigned int subcount = subsymbol->Length();

	this->set_text(symbol + subsymbol + suffix);
	this->set_layout_font_size(symcount, subcount);
	this->set_layout_font_style(symcount, subcount, FontStyle::Italic);
}

void Textlet::set_text(const wchar_t *fmt, ...) {
	VSWPRINT(content, fmt);
	this->set_text(content);
}

void Textlet::set_layout_font_size(int char_idx, int char_count) {
	this->set_layout_font_size(char_idx, char_count, this->subscript_fontsize);
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

Labellet::Labellet(Platform::String^ caption, CanvasTextFormat^ font, ICanvasBrush^ color) {
	if (font != nullptr) {
		this->set_font(font);
	}

	if (color != nullptr) {
		this->set_color(color);
	}

	this->set_text(caption);
}

Labellet::Labellet(Platform::String^ caption, Platform::String^ subscript, CanvasTextFormat^ font, ICanvasBrush^ color) {
	if (font != nullptr) {
		this->set_font(font);
	}

	if (color != nullptr) {
		this->set_color(color);
	}

	this->set_subtext(caption, subscript);
}

Labellet::Labellet(Platform::String^ caption, CanvasTextFormat^ font, unsigned int color_hex, double alpha) {
	if (font != nullptr) {
		this->set_font(font);
	}

	this->set_color(color_hex, alpha);
	this->set_text(caption);
}

Labellet::Labellet(Platform::String^ caption, Platform::String^ subscript, CanvasTextFormat^ font, unsigned int color_hex, double alpha) {
	if (font != nullptr) {
		this->set_font(font);
	}

	this->set_color(color_hex, alpha);
	this->set_subtext(caption, subscript);
}

Labellet::Labellet(Platform::String^ caption, ICanvasBrush^ color) {
	if (color != nullptr) {
		this->set_color(color);
	}

	this->set_text(caption);
}

Labellet::Labellet(Platform::String^ caption, Platform::String^ subscript, ICanvasBrush^ color) {
	if (color != nullptr) {
		this->set_color(color);
	}

	this->set_subtext(caption, subscript);
}

Labellet::Labellet(Platform::String^ caption, unsigned int color_hex, double alpha) {
	this->set_color(color_hex, alpha);
	this->set_text(caption);
}

Labellet::Labellet(Platform::String^ caption, Platform::String^ subscript, unsigned int color_hex, double alpha) {
	this->set_color(color_hex, alpha);
	this->set_subtext(caption, subscript);
}

/*************************************************************************************************/
Dimensionlet::Dimensionlet(Platform::String^ unit, CanvasTextFormat^ nfont, CanvasTextFormat^ lfont, ICanvasBrush^ color)
	: Dimensionlet(unit, "", nfont, lfont, color) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, Platform::String^ label, CanvasTextFormat^ nfont, CanvasTextFormat^ lfont, ICanvasBrush^ color)
	: Dimensionlet(unit, label, "", nfont, lfont, color, color) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, CanvasTextFormat^ font, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: Dimensionlet(unit, "", font, ncolor, lcolor) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, Platform::String^ label, CanvasTextFormat^ font, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: Dimensionlet(unit, label, "", font, font, ncolor, lcolor) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, Platform::String^ label, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: Dimensionlet(unit, label, nullptr, ncolor, lcolor) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: Dimensionlet(unit, "", ncolor, lcolor) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, CanvasTextFormat^ font, ICanvasBrush^ color)
	: Dimensionlet(unit, "", font, color) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, Platform::String^ label, CanvasTextFormat^ font, ICanvasBrush^ color)
	: Dimensionlet(unit, label, "", font, font, color, color) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, Platform::String^ label, Platform::String^ subscript
	, CanvasTextFormat^ nfont, CanvasTextFormat^ lfont, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: num_color(ncolor), num_font((nfont == nullptr) ? default_math_font : nfont) {
	this->set_color((lcolor == nullptr) ? ncolor : lcolor);
	this->set_font((lfont == nullptr) ? default_text_font : lfont);
	this->unit_layout = make_text_layout(unit_speak(unit), this->text_font);
	this->unit_box = get_text_extent(this->unit_layout);

	if (label != nullptr) {
		Platform::String^ symbol = speak(label);
		Platform::String^ suffix = ":";

		if (subscript == nullptr) {
			this->set_text(symbol + suffix);
		} else {
			this->set_subtext(symbol, speak(subscript), suffix);
		}
	}
}

void Dimensionlet::construct() {
	this->set_value(0.0F, true);
}

void Dimensionlet::fill_extent(float x, float y, float* w, float* h) {
	do_fill_extent(this->text_layout, this->num_box, this->unit_box, x, y, w, h);
}

void Dimensionlet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	do_fill_margin(this->text_layout, this->num_box, this->unit_box, t, r, b, l);
}

void Dimensionlet::on_value_changed(float value) {	
	this->num_layout = make_text_layout(scalar(value, this->text_layout == nullptr), this->num_font);
	this->num_box = get_text_extent(this->num_layout);
}

void Dimensionlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->text_color == nullptr) {
		this->set_color();
	}

	if (this->num_color == nullptr) {
		this->num_color = this->text_color;
	}

	draw_metrics(ds, x, y, this->num_box, this->unit_box,
		this->text_layout, this->num_layout, this->unit_layout,
		this->num_color, this->text_color);
}

/*************************************************************************************************/
Percentagelet::Percentagelet(CanvasTextFormat^ nfont, CanvasTextFormat^ lfont, ICanvasBrush^ color)
	: Percentagelet("", nfont, lfont, color) {}

Percentagelet::Percentagelet(Platform::String^ label, CanvasTextFormat^ nfont, CanvasTextFormat^ lfont, ICanvasBrush^ color)
	: Percentagelet(label, "", nfont, lfont, color, color) {}

Percentagelet::Percentagelet(ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: Percentagelet("", ncolor, lcolor) {}

Percentagelet::Percentagelet(Platform::String^ label, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: Percentagelet(label, nullptr, ncolor, lcolor) {}

Percentagelet::Percentagelet(CanvasTextFormat^ font, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: Percentagelet("", font, ncolor, lcolor) {}

Percentagelet::Percentagelet(Platform::String^ label, CanvasTextFormat^ font, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: Percentagelet(label, "", font, font, ncolor, lcolor) {}

Percentagelet::Percentagelet(CanvasTextFormat^ font, ICanvasBrush^ color)
	: Percentagelet("", font, color) {}

Percentagelet::Percentagelet(Platform::String^ label, CanvasTextFormat^ font, ICanvasBrush^ color)
	: Percentagelet(label, "", font, font, color, color) {}

Percentagelet::Percentagelet(Platform::String^ label, Platform::String^ subscript
	, CanvasTextFormat^ nfont, CanvasTextFormat^ lfont, ICanvasBrush^ ncolor, ICanvasBrush^ lcolor)
	: num_color(ncolor), num_font((nfont == nullptr) ? default_math_font : nfont) {
	this->set_color((lcolor == nullptr) ? ncolor : lcolor);
	this->set_font((lfont == nullptr) ? default_text_font : lfont);
	this->sign_layout = make_text_layout("%", this->text_font);
	this->sign_box = get_text_extent(this->sign_layout);

	if (label != nullptr) {
		Platform::String^ symbol = speak(label);
		Platform::String^ suffix = ":";

		if (subscript == nullptr) {
			this->set_text(symbol + suffix);
		} else {
			this->set_subtext(symbol, speak(subscript), suffix);
		}
	}
}

void Percentagelet::construct() {
	this->set_value(0.0F, true);
}

void Percentagelet::fill_extent(float x, float y, float* w, float* h) {
	do_fill_extent(this->text_layout, this->num_box, this->sign_box, x, y, w, h);
}

void Percentagelet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	do_fill_margin(this->text_layout, this->num_box, this->sign_box, t, r, b, l);
}

void Percentagelet::on_value_changed(float value) {
	this->num_layout = make_text_layout(scalar(value, this->text_layout == nullptr), this->num_font);
	this->num_box = get_text_extent(this->num_layout);
}

void Percentagelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->text_color == nullptr) {
		this->set_color();
	}

	if (this->num_color == nullptr) {
		this->num_color = this->text_color;
	}

	draw_metrics(ds, x, y, this->num_box, this->sign_box,
		this->text_layout, this->num_layout, this->sign_layout,
		this->num_color, this->text_color);
}
