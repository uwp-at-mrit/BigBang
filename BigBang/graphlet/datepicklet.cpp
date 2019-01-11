#include <cwchar>

#include "graphlet/datepicklet.hpp"

#include "forward.hpp"
#include "paint.hpp"
#include "tongue.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;
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

static inline Platform::String^ accumulate_number(Platform::String^ src, double acc_percentage, long double maximum) {
	long double acc = acc_percentage * (std::isnan(maximum) ? 100.0 : maximum);
	
	return (std::wcstold(src->Data(), nullptr) + acc).ToString();
}

/*************************************************************************************************/
DatePicklet::DatePicklet(DatePickerState status, DatePickerStyle& style, Platform::String^ unit, Platform::String^ label, Platform::String^ subscript)
	: DatePicklet(status, unit, label, subscript) {
	
	/** TODO: Why does not it work if pass the `style` to IStatelet */
	this->set_style(style);
}

DatePicklet::DatePicklet(DatePickerState status, Platform::String^ unit, Platform::String^ label, Platform::String^ subscript)
	: IStatelet(status), unit(unit), maximum(std::nanl("infinity")) {
	/** TODO: Why does not it work if pass the `status` to IStatelet */
	this->set_state(status);
}

void DatePicklet::construct() {
}

void DatePicklet::update(long long count, long long interval, long long uptime) {
	if (count % 2 == 0) {
		if (this->has_caret()) {
			this->flashing = !this->flashing;
			this->notify_updated();
		}
	}
}

void DatePicklet::fill_extent(float x, float y, float* w, float* h) {
	DatePickerStyle style = this->get_style();

	if (w != nullptr) {
		TextExtent nbox = (this->has_caret() ? this->caret_box : this->number_box);
		
		(*w) = std::fmaxf(nbox.width, style.minimize_number_width) + this->unit_box.width;

		if (this->unit_layout != nullptr) {
			(*w) += style.number_trailing_space;
		}
	}

	if (h != nullptr) {
		TextExtent label_box;
		float tspace, bspace;
	}
}

void DatePicklet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	TextExtent label_box;
	float tspace, bspace;

	SET_VALUES(l, label_box.lspace, r, this->unit_box.rspace);
	SET_VALUES(t, tspace, b, bspace);
}

/*
void DatePicklet::on_value_changed(double value) {
	DatePickerStyle style = this->get_style();

	this->number = flstring(value, style.precision);
	this->number_layout = make_text_layout(this->number, style.number_font);
	this->number_box = get_text_extent(this->number_layout);
}
*/

void DatePicklet::on_state_changed(DatePickerState status) {
	this->enable_events(status == DatePickerState::Input);
}

bool DatePicklet::on_key(VirtualKey key, bool wargrey_keyboard) {
	static unsigned int num0 = static_cast<unsigned int>(VirtualKey::Number0);
	static unsigned int pad0 = static_cast<unsigned int>(VirtualKey::NumberPad0);
	unsigned int keycode = static_cast<unsigned int>(key);
	bool handled = false;

	switch (key) {
	case VirtualKey::Subtract: {
		if (this->input_number == nullptr) {
			this->input_number = "-";
		}
		handled = true;
	}; break;
	case VirtualKey::Decimal: {
		if (this->decimal_position < 0) {
			this->decimal_position = this->input_number->Length() - 1;
			this->input_number += ".";
		}
		handled = true;
	}; break;
	case VirtualKey::PageUp: {
		this->input_number = accumulate_number(this->input_number, 0.10, this->maximum);
		handled = true;
	}; break;
	case VirtualKey::Up: {
		this->input_number = accumulate_number(this->input_number, 0.01, this->maximum);
		handled = true;
	}; break;
	case VirtualKey::PageDown: {
		this->input_number = accumulate_number(this->input_number, -0.10, this->maximum);
		handled = true;
	}; break;
	case VirtualKey::Down: {
		this->input_number = accumulate_number(this->input_number, -0.01, this->maximum);
		handled = true;
	}; break;
	case VirtualKey::Back: {
		if (this->input_number != nullptr) {
			int count = this->input_number->Length() - 1;

			this->input_number = ref new Platform::String(this->input_number->Data(), count);

			if (count < this->decimal_position) {
				this->decimal_position = -1;
			}
		}
		handled = true;
	}; break;
	default: {
		if ((VirtualKey::Number0 <= key) && (key <= VirtualKey::Number9)) {
			this->input_number += (keycode - num0).ToString();
			handled = true;
		} else if ((VirtualKey::NumberPad0 <= key) && (key <= VirtualKey::NumberPad9)) {
			this->input_number += (keycode - pad0).ToString();
			handled = true;
		}
	}
	}

	if (handled) {
		DatePickerStyle style = this->get_style();

		this->caret_layout = make_text_layout(this->input_number, style.number_font);
		this->caret_box = get_text_extent(this->caret_layout);

		this->notify_updated();
	}

	return handled;
}

void DatePicklet::own_caret(bool on) {
	this->flashing = on;

	if (on) {
		this->input_number = nullptr;
		this->caret_layout = nullptr;
		this->caret_box = TextExtent();
		this->decimal_position = -1;
	}
}

void DatePicklet::set_maximum(long double maximum) {
	this->maximum = maximum;
}

void DatePicklet::set_input_number(long double n, int precision) {
	if (!std::isnan(this->maximum)) {
		n = std::fminl(n, this->maximum);
	}

	this->input_number = flstring(n, precision);
}

long double DatePicklet::get_input_number() {
	long double v = 0.0;

	if (this->input_number != nullptr) {
		v = std::wcstold(this->input_number->Data(), nullptr);
	}

	if (!std::isnan(this->maximum)) {
		v = std::fminl(v, this->maximum);
	}

	return v;
}

void DatePicklet::prepare_style(DatePickerState status, DatePickerStyle& style) {
	CAS_SLOT(style.number_color, default_number_color);
	CAS_SLOT(style.unit_color, default_unit_color);
	CAS_SLOT(style.label_color, style.unit_color);

	CAS_SLOT(style.number_font, default_math_font);
	CAS_SLOT(style.unit_font, default_unit_font);
	CAS_SLOT(style.label_font, style.unit_font);

	CAS_SLOT(style.caret_color, Colours::Foreground); 

	FLCAS_SLOT(style.minimize_label_width, 0.0F);
	FLCAS_SLOT(style.label_xfraction, 1.0F);
	FLCAS_SLOT(style.number_leading_space, get_text_extent("0", style.number_font).width);

	switch (status) {
	case DatePickerState::Input: {
		FLCAS_SLOT(style.minimize_number_width, get_text_extent("-123456.7890", style.number_font).width);
		FLCAS_SLOT(style.number_xfraction, 0.0F);
		FLCAS_SLOT(style.number_trailing_space, 0.0F);
		ICAS_SLOT(style.precision, 0);
	}; break;
	case DatePickerState::Normal: case DatePickerState::Highlight: {
		FLCAS_SLOT(style.minimize_number_width, 0.0F);
		FLCAS_SLOT(style.number_xfraction, 0.5F);
		FLCAS_SLOT(style.number_trailing_space, 0.0F);
		ICAS_SLOT(style.precision, 1);
	}; break;
	}

	// NOTE: the others can be `nullptr`
}

void DatePicklet::apply_style(DatePickerStyle& style) {
	this->number_layout = make_text_layout(this->number, style.number_font);
	this->number_box = get_text_extent(this->number_layout);
	this->number09_box = get_text_extent("0123456789", style.number_font);

	if (this->unit != nullptr) {
		this->unit_layout = make_text_layout(this->unit, style.unit_font);
		this->unit_box = get_text_extent(this->unit_layout);
	}
}

void DatePicklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	DatePickerStyle style = this->get_style();
	CanvasTextLayout^ nlayout = this->number_layout;
	TextExtent nbox = this->number_box;
	TextExtent label_box;
	float tspace, bspace, height, base_y;
	float number_region_x = 0.0F;
	float number_x = -nbox.width;
	
	base_y = y + height;

	if (this->has_caret()) {
		nbox = this->caret_box;
		nlayout = this->caret_layout;
	}

	{ // draw number
		float region_width = std::fmaxf(nbox.width, style.minimize_number_width);
		float padding_x = ((style.number_border_color != nullptr) ? 1.0F : 0.0F);
		
		if (style.number_background_color != nullptr) {
			ds->FillRectangle(x, y, region_width, height, style.number_background_color);
		}

		if (nlayout != nullptr) {
			number_x = x + (region_width - nbox.width) * style.number_xfraction + padding_x;
			ds->DrawTextLayout(nlayout, number_x, base_y - this->number_box.height, style.number_color);
		}

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
		float padding_x = 3.0F;
		float caret_x = std::fmaxf(number_region_x + padding_x, number_x + nbox.width);

		ds->DrawLine(caret_x, y + padding_x, caret_x, y + height - padding_x, style.caret_color);
	}
}
