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

static inline Platform::String^ scalar(double value, int precision) {
	return ((precision > 0)
		? make_wstring(make_wstring(L"%%.%dlf", precision)->Data(), value)
		: value.ToString());
}

static void fill_vmetrics(CanvasTextLayout^ layout, TextExtent& num_box, TextExtent& unit_box
	, TextExtent* label_box, float* tspace, float* bspace, float* height = nullptr) {
	(*label_box) = ((layout == nullptr) ? num_box : get_text_extent(layout));
	(*tspace) = std::fminf(label_box->tspace, std::fminf(num_box.tspace, unit_box.tspace));
	(*bspace) = std::fminf(label_box->bspace, std::fminf(num_box.bspace, unit_box.bspace));

	if (height != nullptr) {
		float link = label_box->height - label_box->tspace - unit_box.bspace;
		float nink = num_box.height - num_box.tspace - unit_box.bspace;
		float uink = unit_box.height - unit_box.tspace - unit_box.bspace;
		float ink_height = std::fmaxf(std::fmaxf(nink, uink), link);

		(*height) = (*tspace) + ink_height + (*bspace);
	}
}

/*************************************************************************************************/
DimensionStyle WarGrey::SCADA::make_setting_dimension_style(float nfsize, unsigned int min_number) {
	auto nf = make_bold_text_format("Cambria Math", nfsize);
	auto uf = make_bold_text_format("Cambria", nfsize);
	ICanvasBrush^ color = Colours::GhostWhite;
	DimensionStyle ds;

	ds.number_font = nf;
	ds.unit_font = uf;
	ds.minimize_number_width = get_text_extent("0", nf).width * float(min_number);
	ds.number_border_color = color;
	ds.number_color = color;
	ds.unit_color = color;
	ds.caret_color = color;
	
	return ds;
}

DimensionStyle WarGrey::SCADA::make_highlight_dimension_style(float nfsize, unsigned int min_number) {
	auto nf = make_bold_text_format("Cambria Math", nfsize);
	auto uf = make_bold_text_format("Cambria", nfsize * 0.90F);
	DimensionStyle ds;

	ds.number_font = nf;
	ds.unit_font = uf;
	ds.minimize_label_width = get_text_extent("O", uf).height;
	ds.label_xfraction = 0.5F;
	ds.minimize_number_width = get_text_extent("0", nf).width * float(min_number);
	ds.number_background_color = Colours::Gray;
	ds.number_color = Colours::Background;
	ds.label_background_color = Colours::ForestGreen;
	ds.label_color = Colours::GhostWhite;
	ds.number_leading_space = 2.0F;

	return ds;
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

	/** TODO: Why it does not work if pass the `style` to IStatuslet */
	this->set_style(style);
}

IEditorlet::IEditorlet(EditorStatus status, Platform::String^ unit, Platform::String^ label, Platform::String^ subscript)
	: IStatuslet(status), unit(unit) {
	
	this->set_text(speak(label), speak(subscript));
}

void IEditorlet::construct() {
	this->set_value(0.0, true);
}

void IEditorlet::update(long long count, long long interval, long long uptime) {
	if (count % 2 == 0) {
		if (this->get_status() == EditorStatus::Enabled) {
			this->flashing = !this->flashing;
			this->notify_updated();
		}
	}
}

void IEditorlet::fill_extent(float x, float y, float* w, float* h) {
	DimensionStyle style = this->get_style();

	if (w != nullptr) {
		(*w) = std::fmaxf(this->number_box.width, style.minimize_number_width) + this->unit_box.width;

		if (this->text_layout != nullptr) {
			(*w) += std::fmaxf(this->text_layout->LayoutBounds.Width, style.minimize_label_width);
			(*w) += style.number_leading_space;
		}

		if (this->unit_layout != nullptr) {
			(*w) += style.number_trailing_space;
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
	
	if (this->text_layout == nullptr) {
		DimensionStyle style = this->get_style();
		float region_width = std::fmaxf(this->number_box.width, style.minimize_number_width);
		
		label_box.lspace = (region_width - this->number_box.width) * style.number_xfraction + this->number_box.lspace;
	} else {
		DimensionStyle style = this->get_style();
		float region_width = std::fmaxf(label_box.width, style.minimize_label_width);
		
		label_box.lspace += ((region_width - label_box.width) * style.label_xfraction);
	}



	SET_VALUES(l, label_box.lspace, r, this->unit_box.rspace);
	SET_VALUES(t, tspace, b, bspace);
}

void IEditorlet::on_value_changed(double value) {
	DimensionStyle style = this->get_style();

	this->number = scalar(value, style.precision);
	this->number_layout = make_text_layout(this->number, style.number_font);
	this->number_box = get_text_extent(this->number_layout);
}

void IEditorlet::on_status_changed(EditorStatus status) {
	this->flashing = (status == EditorStatus::Enabled);
}

void IEditorlet::prepare_style(EditorStatus status, DimensionStyle& style) {
	CAS_SLOT(style.number_color, default_number_color);
	CAS_SLOT(style.unit_color, default_unit_color);
	CAS_SLOT(style.label_color, style.unit_color);

	CAS_SLOT(style.number_font, default_math_font);
	CAS_SLOT(style.unit_font, default_unit_font);
	CAS_SLOT(style.label_font, style.unit_font);

	CAS_SLOT(style.caret_color, Colours::Foreground); 
	
	FLCAS_SLOT(style.minimize_label_width, 0.0F);
	FLCAS_SLOT(style.label_xfraction, 1.0F);

	switch (status) {
	case EditorStatus::Enabled: {
		FLCAS_SLOT(style.minimize_number_width, get_text_extent("123456.789", style.number_font).width);
		FLCAS_SLOT(style.number_xfraction, 0.0F);
		FLCAS_SLOT(style.number_leading_space, get_text_extent("0", style.number_font).width);
		FLCAS_SLOT(style.number_trailing_space, 0.0F);
		style.precision = 0;
	}; break;
	case EditorStatus::Disabled: {
		FLCAS_SLOT(style.minimize_number_width, 0.0F);
		FLCAS_SLOT(style.number_xfraction, 0.5F);
		FLCAS_SLOT(style.number_leading_space, get_text_extent("0", style.number_font).width);
		FLCAS_SLOT(style.number_trailing_space, 0.0F);
		ICAS_SLOT(style.precision, 1);
	}; break;
	}

	// NOTE: the others can be `nullptr`
}

void IEditorlet::apply_style(DimensionStyle& style) {
	this->set_color(style.label_color);
	this->set_font(style.label_font);

	this->number_layout = make_text_layout(this->number, style.number_font);
	this->number_box = get_text_extent(this->number_layout);

	if (this->unit != nullptr) {
		this->unit_layout = make_text_layout(this->unit, style.unit_font);
		this->unit_box = get_text_extent(this->unit_layout);
	}
}

void IEditorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	DimensionStyle style = this->get_style();
	TextExtent label_box;
	float tspace, bspace, height, base_y, box_height;
	
	fill_vmetrics(this->text_layout, this->number_box, this->unit_box, &label_box, &tspace, &bspace, &height);
	base_y = y + height;
	box_height = height - tspace - bspace;

	if (this->text_layout != nullptr) {
		float region_width = std::fmaxf(label_box.width, style.minimize_label_width);
		float label_x = x + (region_width - label_box.width) * style.label_xfraction;

		if (style.label_background_color != nullptr) {
			ds->FillRectangle(x, y, region_width, height, style.label_background_color);
		}
		
		ds->DrawTextLayout(this->text_layout, label_x, base_y - label_box.height, style.label_color);
		
		if (style.label_border_color != nullptr) {
			ds->DrawRectangle(x + 0.5F, y + 0.5F, region_width - 1.0F, height - 1.0F, style.label_border_color);
		}
		
		x += (region_width + style.number_leading_space);
	}

	{ // draw number
		float region_width = std::fmaxf(this->number_box.width, style.minimize_number_width);
		float number_x = x + (region_width - this->number_box.width) * style.number_xfraction;

		if (style.number_background_color != nullptr) {
			ds->FillRectangle(x, y, region_width, height, style.number_background_color);
		}

		ds->DrawTextLayout(this->number_layout, x, base_y - number_box.height, style.number_color);

		if (style.number_border_color != nullptr) {
			ds->DrawRectangle(x + 0.5F, y + 0.5F, region_width - 1.0F, height - 1.0F, style.number_border_color);
		}

		x += region_width;
	}

	if (this->unit_layout != nullptr) {
		x += style.number_trailing_space;

		if (style.unit_background_color != nullptr) {
			ds->FillRectangle(x, y, unit_box.width, height, style.unit_background_color);
		}

		ds->DrawTextLayout(this->unit_layout, x, base_y - unit_box.height, style.unit_color);

		if (style.unit_border_color != nullptr) {
			ds->DrawRectangle(x + 0.5F, y + 0.5F, unit_box.width - 1.0F, height - 1.0F, style.unit_border_color);
		}
	}

	if (this->flashing) {
		ds->DrawLine(x + 2.0F, y + 2.0F, x + 2.0F, y + height - 4.0F, style.caret_color);
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

/*************************************************************************************************/
Percentagelet::Percentagelet(EditorStatus default_status, DimensionStyle& default_style
	, Platform::String^ label, Platform::String^ subscript)
	: IEditorlet(default_status, default_style, "%", label, subscript) {}

Percentagelet::Percentagelet(DimensionStyle& default_style, Platform::String^ label, Platform::String^ subscript)
	: Percentagelet(EditorStatus::Disabled, default_style, label, subscript) {}

Percentagelet::Percentagelet(EditorStatus default_status, Platform::String^ label, Platform::String^ subscript)
	: IEditorlet(default_status, "%", label, subscript) {}

Percentagelet::Percentagelet(Platform::String^ label, Platform::String^ subscript)
	: Percentagelet(EditorStatus::Disabled, label, subscript) {}
