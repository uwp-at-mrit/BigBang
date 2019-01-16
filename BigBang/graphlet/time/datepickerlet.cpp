#include "graphlet/time/datepickerlet.hpp"

#include "time.hpp"
#include "string.hpp"

#include "paint.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;
using namespace Windows::UI::Text;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum DateTimeIndex { Year, Month, Day, Hour, Minute, Second, _ };

static CanvasTextFormat^ default_label_font = make_bold_text_format();
static CanvasTextFormat^ default_datetime_font = make_bold_text_format("Consolas", 16.0F);

static ICanvasBrush^ default_label_color = Colours::make(0x23EBB9U);
static ICanvasBrush^ default_caret_color = Colours::DodgerBlue;
static ICanvasBrush^ default_datetime_color = Colours::Silver;

static DatePickerState default_date_picker_status = DatePickerState::Default;

static void fill_vmetrics(CanvasTextLayout^ layout, TextExtent& dt_box, TextExtent& ds_box, TextExtent& ts_box
	, TextExtent* label_box, float* tspace, float* bspace, float* height = nullptr) {
	(*label_box) = ((layout == nullptr) ? dt_box : get_text_extent(layout));
	(*tspace) = std::fminf(label_box->tspace, std::fminf(dt_box.tspace, std::fminf(ds_box.tspace, ts_box.tspace)));
	(*bspace) = std::fminf(label_box->bspace, std::fminf(dt_box.bspace, std::fminf(ds_box.bspace, ts_box.bspace)));

	if (height != nullptr) {
		float link = label_box->height - label_box->tspace - label_box->bspace;
		float dtink = dt_box.height - dt_box.tspace - dt_box.bspace;
		float dsink = ds_box.height - ds_box.tspace - ds_box.bspace;
		float tsink = ts_box.height - ts_box.tspace - ts_box.bspace;
		float ink_height = std::fmaxf(std::fmaxf(dtink, std::fmaxf(dsink, tsink)), link);

		(*height) = (*tspace) + ink_height + (*bspace);
	}
}

static inline float box_cc(TextExtent* te) {
	return te->height * 0.5F - (te->tspace - te->bspace) * 0.5F;
}

/*************************************************************************************************/
DatePickerlet::DatePickerlet(DatePickerState status, DatePickerStyle& style, long long timepoint, Platform::String^ label, Platform::String^ subscript)
	: DatePickerlet(status, timepoint, label, subscript) {
	/** TODO: Why does not it work if pass the `style` to IStatelet */
	this->set_style(style);
}

DatePickerlet::DatePickerlet(DatePickerState status, long long timepoint, Platform::String^ label, Platform::String^ subscript)
	: IStatelet(status), caret_index(2 /* the day */) {
	this->set_text(label, subscript);
	this->timepoint0 = timepoint;

	/** TODO: Why does not it work if pass the `status` to IStatelet */
	this->set_state(status);
}

DatePickerlet::DatePickerlet(DatePickerStyle& style, long long timepoint, Platform::String^ label, Platform::String^ subscript)
	: DatePickerlet(default_date_picker_status, style, timepoint, label, subscript) {}

DatePickerlet::DatePickerlet(long long timepoint, Platform::String^ label, Platform::String^ subscript)
	: DatePickerlet(default_date_picker_status, timepoint, label, subscript) {}

long long DatePickerlet::guarded_value(long long timepoint0) {
	long long timepoint = timepoint0;

	if (timepoint <= 0) {
		timepoint = current_seconds() + timepoint;
	}

	return timepoint;
}

void DatePickerlet::construct() {
	this->set_value(this->timepoint0, true);
}

void DatePickerlet::fill_extent(float x, float y, float* w, float* h) {
	DatePickerStyle style = this->get_style();

	if (w != nullptr) {
		(*w) = this->date_separator_box.width * 3.0F + this->time_separator_box.width * 2.0F;

		for (unsigned int idx = 0; idx < DateTimeIndex::_; idx++) {
			(*w) += this->datetime_boxes[idx].width;
		}

		if (this->text_layout != nullptr) {
			(*w) += std::fmaxf(this->text_layout->LayoutBounds.Width, style.minimize_label_width);
			(*w) += style.datetime_leading_space;
		}
	}

	if (h != nullptr) {
		TextExtent label_box;
		float tspace, bspace;

		fill_vmetrics(this->text_layout, this->number09_box, this->date_separator_box, this->time_separator_box,
			&label_box, &tspace, &bspace, h);
	}
}

void DatePickerlet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	TextExtent label_box;
	float tspace, bspace;

	fill_vmetrics(this->text_layout, this->number09_box, this->date_separator_box, this->time_separator_box,
		&label_box, &tspace, &bspace);

	if (this->text_layout != nullptr) {
		DatePickerStyle style = this->get_style();
		float region_width = std::fmaxf(label_box.width, style.minimize_label_width);

		label_box.lspace += ((region_width - label_box.width) * style.label_xfraction);
	} else {
		label_box.lspace = this->datetime_boxes[0].lspace;
	}

	SET_VALUES(l, label_box.lspace, r, this->datetime_boxes[DateTimeIndex::Second].rspace);
	SET_VALUES(t, tspace, b, bspace);
}

void DatePickerlet::on_state_changed(DatePickerState status) {
	this->enable_events(status == DatePickerState::Input);
}

bool DatePickerlet::on_key(VirtualKey key, bool wargrey_keyboard) {
	bool handled = true;

	switch (key) {
	case VirtualKey::Escape: this->set_value(this->timepoint0); break;
	case VirtualKey::Left: this->caret_index = (this->caret_index + DateTimeIndex::Second) % DateTimeIndex::_; break;
	case VirtualKey::Right: this->caret_index = (this->caret_index + 1) % DateTimeIndex::_; break;
	case VirtualKey::Up: case VirtualKey::Down: {
		int sign = ((key == VirtualKey::Up) ? -1 : 1);
		long long s = this->get_value();
		
		switch (this->caret_index) {
		case DateTimeIndex::Year: this->set_value(seconds_add_years(s, sign)); break;
		case DateTimeIndex::Month: this->set_value(seconds_add_months(s, sign)); break;
		case DateTimeIndex::Day: this->set_value(seconds_add_days(s, sign)); break;
		case DateTimeIndex::Hour: this->set_value(seconds_add_hours(s, sign)); break;
		case DateTimeIndex::Minute: this->set_value(seconds_add_minutes(s, sign)); break;
		case DateTimeIndex::Second: this->set_value(seconds_add_seconds(s, sign)); break;
		}
	}; break;
	case VirtualKey::PageUp: case VirtualKey::PageDown: {
		int sign = ((key == VirtualKey::PageUp) ? -1 : 1);
		long long s = this->get_value();

		switch (this->caret_index) {
		case DateTimeIndex::Year: this->set_value(seconds_add_years(s, 4 * sign)); break;
		case DateTimeIndex::Month: this->set_value(seconds_add_months(s, 3 * sign)); break;
		case DateTimeIndex::Day: this->set_value(seconds_add_days(s, 7 * sign)); break;
		case DateTimeIndex::Hour: this->set_value(seconds_add_hours(s, 4 * sign)); break;
		case DateTimeIndex::Minute: this->set_value(seconds_add_minutes(s, 10 * sign)); break;
		case DateTimeIndex::Second: this->set_value(seconds_add_seconds(s, 10 * sign)); break;
		}
	}; break;
	default: handled = false; break;
	}

	if (handled) {
		this->notify_updated();
	}

	return handled;
}

void DatePickerlet::prepare_style(DatePickerState status, DatePickerStyle& style) {
	CAS_SLOT(style.label_color, default_label_color);
	CAS_SLOT(style.label_font, default_label_font);

	CAS_SLOT(style.datetime_color, default_datetime_color);
	CAS_SLOT(style.datetime_font, default_datetime_font);

	CAS_SLOT(style.separator_color, style.datetime_color);
	CAS_SLOT(style.separator_font, style.datetime_font);
	
	CAS_SLOT(style.caret_color, default_caret_color);

	FLCAS_SLOT(style.minimize_label_width, 0.0F);
	FLCAS_SLOT(style.label_xfraction, 1.0F);
	FLCAS_SLOT(style.datetime_leading_space, get_text_extent("0", style.datetime_font).width);
	
	// NOTE: the others can be `nullptr`
}

void DatePickerlet::apply_style(DatePickerStyle& style) {
	long long timepoint = this->get_value();

	this->set_color(style.label_color);
	this->set_font(style.label_font);

	if (timepoint > 0LL) {
		this->on_value_changed(timepoint);
	}
}

void DatePickerlet::on_value_changed(long long timepoint) {
	DatePickerStyle style = this->get_style();

	this->date_separator_layout = make_text_layout("-", style.separator_font);
	this->date_separator_box = get_text_extent(this->date_separator_layout);

	this->time_separator_layout = make_text_layout(":", style.separator_font);
	this->time_separator_box = get_text_extent(this->time_separator_layout);

	this->number09_box = get_text_extent("0123456789", style.datetime_font);

	{ // remake datetime layouts
		long long datetime[6];

		split_date_utc(timepoint, true, &datetime[0], &datetime[1], &datetime[2]);
		split_time_utc(timepoint, true, &datetime[3], &datetime[4], &datetime[5]);

		for (unsigned int idx = 0; idx < DateTimeIndex::_; idx++) {
			unsigned int precision = ((idx == 0) ? 4 : 2);

			this->datetime_layouts[idx] = make_text_layout(fxstring(datetime[idx], precision), style.datetime_font);
			this->datetime_boxes[idx] = get_text_extent(this->datetime_layouts[idx]);
		}
	}
}

void DatePickerlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	bool focused = this->has_caret();
	DatePickerStyle style = this->get_style();
	TextExtent label_box;
	float tspace, bspace, height, center_y;
	float datetime_x = 0.0F;

	fill_vmetrics(this->text_layout, this->number09_box, this->date_separator_box, this->time_separator_box,
		&label_box, &tspace, &bspace, &height);

	center_y = y + height * 0.5F;

	if (this->text_layout != nullptr) {
		float region_width = std::fmaxf(label_box.width, style.minimize_label_width);
		float label_x = x + (region_width - label_box.width) * style.label_xfraction;

		if (style.label_background_color != nullptr) {
			ds->FillRectangle(x, y, region_width, height, style.label_background_color);
		}

		ds->DrawTextLayout(this->text_layout, label_x, center_y - box_cc(&label_box), style.label_color);

		if (style.label_border_color != nullptr) {
			ds->DrawRectangle(x + 0.5F, y + 0.5F, region_width - 1.0F, height - 1.0F, style.label_border_color);
		}

		x += (region_width + style.datetime_leading_space);
		datetime_x = x;
	}

	for (unsigned int idx = 0; idx < DateTimeIndex::_; idx++) {
		float region_width = this->datetime_boxes[idx].width;
		float padding_x = ((style.datetime_border_color != nullptr) ? 1.0F : 0.0F);
		ICanvasBrush^ foreground_color = style.datetime_color;
		CanvasTextLayout^ separator = nullptr;
		TextExtent* box = nullptr;

		if (style.datetime_background_color != nullptr) {
			ds->FillRectangle(x, y, region_width, height, style.datetime_background_color);
		}

		if (focused && (idx == this->caret_index) && (style.caret_color != nullptr)) {
			foreground_color = style.caret_color;
		}

		if (this->datetime_layouts[idx] != nullptr) {
			box = &this->datetime_boxes[idx];
			ds->DrawTextLayout(this->datetime_layouts[idx],
				x + padding_x, center_y - box_cc(box),
				foreground_color);
		}

		if (style.datetime_border_color != nullptr) {
			ds->DrawRectangle(x + 0.5F, y + 0.5F, region_width - 1.0F, height - 1.0F, style.datetime_border_color);
		}

		if (idx < DateTimeIndex::Day) {
			separator = this->date_separator_layout;
			box = &this->date_separator_box;
		} else if (idx > DateTimeIndex::Day) {
			separator = this->time_separator_layout;
			box = &this->time_separator_box;
		}

		x += region_width;

		if (separator != nullptr) {
			ds->DrawTextLayout(separator, x, center_y - box->height * 0.5F, style.separator_color);
			x += box->width;
		} else {
			x += this->date_separator_box.width;
		}
	}
}
