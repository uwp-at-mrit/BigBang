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
static CanvasTextFormat^ default_unit_font = make_bold_text_format("Cambria", 14.0F);
static CanvasTextFormat^ default_math_font = make_bold_text_format("Cambria Math", 16.0F);

static ICanvasBrush^ default_text_color = Colours::Silver;
static ICanvasBrush^ default_number_color = Colours::Yellow;
static ICanvasBrush^ default_unit_color = Colours::make(0x23EBB9U);

static inline Platform::String^ unit_speak(Platform::String^ unit) {
	bool exists;
	Platform::String^ dialect = speak(unit, "unit", &exists);

	return (exists ? dialect : speak(unit));
}

static inline Platform::String^ scalar(double value) {
	return value.ToString();
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

/*************************************************************************************************/
void ITextlet::set_color(ICanvasBrush^ color) {
	this->text_color = ((color == nullptr) ? default_text_color : color);
	this->notify_updated();
}

void ITextlet::set_color(unsigned int color_hex, double alpha) {
	this->set_color(Colours::make(color_hex, alpha));
}

void ITextlet::set_font(CanvasTextFormat^ font, GraphletAnchor anchor) {
	if (font == nullptr) {
		this->set_font(default_text_font, anchor);
	} else {
		this->moor(anchor);

		this->text_font = font;
		this->subscript_fontsize = font->FontSize * 0.618F;
		this->set_text(this->raw, this->sub_index, this->sub_count, anchor);
		this->on_font_changed();

		this->notify_updated();
	}
}

void ITextlet::set_text(Platform::String^ content, unsigned int subidx, unsigned int subcount, GraphletAnchor anchor) {
	this->raw = content;
	this->sub_index = subidx;
	this->sub_count = subcount;

	this->moor(anchor);

	if (this->text_font == nullptr) {
		this->set_font(default_text_font);
	} else if (this->raw == nullptr) {
		this->text_layout = nullptr;
	} else {
		this->text_layout = make_text_layout(this->raw, this->text_font);
		this->set_subtext();
	}

	this->notify_updated();
}

void ITextlet::set_text(Platform::String^ content, GraphletAnchor anchor) {
	this->set_text(content, 0, 0, anchor);
}

void ITextlet::set_text(Platform::String^ symbol, Platform::String^ subsymbol, Platform::String^ suffix, GraphletAnchor anchor) {
	this->set_text(symbol + subsymbol + suffix, symbol->Length(), subsymbol->Length(), anchor);
}

void ITextlet::set_subtext() {
	if (this->sub_count > 0) {
		this->set_layout_font_size(this->sub_index, this->sub_count);
		this->set_layout_font_style(this->sub_index, this->sub_count, FontStyle::Italic);
	}
}

void ITextlet::set_text(const wchar_t *fmt, ...) {
	VSWPRINT(content, fmt);
	this->set_text(content);
}

void ITextlet::set_layout_font_size(int char_idx, int char_count) {
	this->set_layout_font_size(char_idx, char_count, this->subscript_fontsize);
}

void ITextlet::set_layout_font_size(int char_idx, int char_count, float size) {
	if (this->text_layout != nullptr) {
		this->text_layout->SetFontSize(char_idx, char_count, size);
	}
}

void ITextlet::set_layout_font_style(int char_idx, int char_count, Windows::UI::Text::FontStyle style) {
	if (this->text_layout != nullptr) {
		this->text_layout->SetFontStyle(char_idx, char_count, style);
	}
}

void ITextlet::fill_extent(float x, float y, float* w, float* h) {
	if (this->text_layout != nullptr) {
		auto box = this->text_layout->LayoutBounds;

		SET_VALUES(w, box.Width, h, box.Height);
	} else {
		SET_BOXES(w, h, 0.0F);
	}
}

void ITextlet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	if (this->text_layout != nullptr) {
		TextExtent te = get_text_extent(this->text_layout);

		SET_VALUES(t, te.tspace, b, te.bspace);
		SET_VALUES(l, te.lspace, r, te.rspace);
	} else {
		IGraphlet::fill_margin(x, y, t, r, b, l);
	}
}

void ITextlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
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
	this->set_font(font);
	this->set_color(color);
	this->set_text(caption);
}

Labellet::Labellet(Platform::String^ caption, Platform::String^ subscript, CanvasTextFormat^ font, ICanvasBrush^ color) {
	this->set_font(font);
	this->set_color(color);
	this->set_text(caption, subscript);
}

Labellet::Labellet(Platform::String^ caption, CanvasTextFormat^ font, unsigned int color_hex, double alpha) {
	this->set_font(font);
	this->set_color(color_hex, alpha);
	this->set_text(caption);
}

Labellet::Labellet(Platform::String^ caption, Platform::String^ subscript, CanvasTextFormat^ font, unsigned int color_hex, double alpha) {
	this->set_font(font);
	this->set_color(color_hex, alpha);
	this->set_text(caption, subscript);
}

Labellet::Labellet(Platform::String^ caption, ICanvasBrush^ color) {
	this->set_color(color);
	this->set_text(caption);
}

Labellet::Labellet(Platform::String^ caption, Platform::String^ subscript, ICanvasBrush^ color) {
	this->set_color(color);
	this->set_text(caption, subscript);
}

Labellet::Labellet(Platform::String^ caption, unsigned int color_hex, double alpha) {
	this->set_color(color_hex, alpha);
	this->set_text(caption);
}

Labellet::Labellet(Platform::String^ caption, Platform::String^ subscript, unsigned int color_hex, double alpha) {
	this->set_color(color_hex, alpha);
	this->set_text(caption, subscript);
}

/*************************************************************************************************/
IEditorlet::IEditorlet(EditorStatus status, DimensionStyle& style, Platform::String^ unit
	, Platform::String^ label, Platform::String^ subscript) : IStatuslet(status), unit(unit) {
	
	this->set_text(speak(label), speak(subscript));
	this->editing = true;

	/** TODO: Why it does not work if pass the `style` to IStatuslet */
	this->set_style(style);
}

IEditorlet::IEditorlet(EditorStatus status, Platform::String^ unit, Platform::String^ label, Platform::String^ subscript)
	: IStatuslet(status), unit(unit) {
	
	this->set_text(speak(label), speak(subscript));
	this->editing = true;
}

void IEditorlet::construct() {
	this->set_value(0.0, true);
}

void IEditorlet::update(long long count, long long interval, long long uptime) {
	if (count % 2 == 0) {
		if (this->editing) {
			this->flashing = !this->flashing;
			this->notify_updated();
		}
	}
}

void IEditorlet::fill_extent(float x, float y, float* w, float* h) {
	if (w != nullptr) {
		(*w) = this->number_box.width + this->unit_box.width + this->gapsize;

		if (this->text_layout != nullptr) {
			(*w) += this->text_layout->LayoutBounds.Width;
		}
	}

	if (h != nullptr) {
		TextExtent label_box;
		float tspace, bspace;

		fill_vmetrics(this->text_layout, this->number_box, this->unit_box, &label_box, &tspace, &bspace, h);
	}
}

void IEditorlet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	TextExtent label_box;
	float tspace, bspace;

	fill_vmetrics(this->text_layout, this->number_box, this->unit_box, &label_box, &tspace, &bspace);

	SET_VALUES(l, label_box.lspace, r, this->unit_box.rspace);
	SET_VALUES(t, tspace, b, bspace);
}

void IEditorlet::on_value_changed(double value) {
	DimensionStyle style = this->get_style();

	this->number = scalar(value);
	this->number_layout = make_text_layout(this->number, style.number_font);
	this->number_box = get_text_extent(this->number_layout);

	if (this->text_layout != nullptr) {
		if (this->gapsize == 0.0F) {
			this->gapsize = this->number_box.width / this->number->Length();
		}
	}
}

void IEditorlet::prepare_style(EditorStatus status, DimensionStyle& style) {
	CAS_SLOT(style.number_color, default_number_color);
	CAS_SLOT(style.unit_color, default_unit_color);
	CAS_SLOT(style.label_color, style.unit_color);

	CAS_SLOT(style.number_font, default_math_font);
	CAS_SLOT(style.unit_font, default_unit_font);
	CAS_SLOT(style.label_font, style.unit_font);
}

void IEditorlet::apply_style(DimensionStyle& style) {
	this->set_color(style.label_color);
	this->set_font(style.label_font);

	this->number_layout = make_text_layout(this->number, style.number_font);
	this->number_box = get_text_extent(this->number_layout);
	
	this->unit_layout = make_text_layout(this->unit, style.unit_font);
	this->unit_box = get_text_extent(this->unit_layout);
}

void IEditorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	DimensionStyle style = this->get_style();
	TextExtent label_box;
	float tspace, bspace, height, base_y;
	float lx = x;

	fill_vmetrics(this->text_layout, this->number_box, this->unit_box, &label_box, &tspace, &bspace, &height);
	base_y = y + height;

	if (text_layout != nullptr) {
		ds->DrawTextLayout(text_layout, x, base_y - label_box.height, style.label_color);
		lx += (label_box.width + this->gapsize);

		ds->DrawRectangle(x, y + 0.5F, label_box.width, height - 1.0F, Colours::GrayText);
	}

	ds->DrawTextLayout(this->number_layout, lx, base_y - number_box.height, style.number_color);
	ds->DrawTextLayout(this->unit_layout, lx + number_box.width, base_y - unit_box.height, style.unit_color);

	ds->DrawRectangle(lx, y + 0.5F, number_box.width, height - 1.0F, Colours::Foreground);

	if (this->editing && this->flashing) {
		ds->DrawLine(lx + 2.0F, y + 2.0F, lx + 2.0F, y + height - 4.0F, Colours::Foreground);
	}
}

/*************************************************************************************************/
Dimensionlet::Dimensionlet(EditorStatus default_status, DimensionStyle& default_style, Platform::String^ unit
	, Platform::String^ label, Platform::String^ subscript)
	: IEditorlet(default_status, default_style, unit_speak(unit), label, subscript) {}

Dimensionlet::Dimensionlet(DimensionStyle& default_style, Platform::String^ unit, Platform::String^ label, Platform::String^ subscript)
	: Dimensionlet(EditorStatus::Disabled, default_style, unit, label, subscript) {}

Dimensionlet::Dimensionlet(EditorStatus default_status, Platform::String^ unit, Platform::String^ label, Platform::String^ subscript)
	: IEditorlet(default_status, unit_speak(unit), label, subscript) {}

Dimensionlet::Dimensionlet(Platform::String^ unit, Platform::String^ label, Platform::String^ subscript)
	: Dimensionlet(EditorStatus::Disabled, unit, label, subscript) {}

Percentagelet::Percentagelet(DimensionStyle& default_style, Platform::String^ label, Platform::String^ subscript)
	: IEditorlet(EditorStatus::Disabled, default_style, "%", label, subscript) {}

Percentagelet::Percentagelet(Platform::String^ label, Platform::String^ subscript)
	: IEditorlet(EditorStatus::Disabled, "%", label, subscript) {}
