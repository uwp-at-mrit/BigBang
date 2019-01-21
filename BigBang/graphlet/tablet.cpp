#include "graphlet/tablet.hpp"

#include "string.hpp"

#include "colorspace.hpp"

#include "text.hpp"
#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ table_default_border_color = Colours::make(0xBBBBBB);
static CanvasTextFormat^ table_default_head_font = make_bold_text_format(16.0F);
static CanvasTextFormat^ table_default_cell_font = make_bold_text_format(14.0F);

static const unsigned int diagnostics_region_background = 0x414141U;
static const unsigned int diagnostics_alarm_background = 0x141414U;

static CanvasSolidColorBrush^ table_default_head_background_color = Colours::make(0x8FBC8F);
static CanvasSolidColorBrush^ table_default_head_foreground_color = Colours::make(0xF8F8FF);

/*************************************************************************************************/
float WarGrey::SCADA::resolve_average_column_width(unsigned int idx, unsigned int column_count) {
	return 1.0F / float(column_count);
}

void WarGrey::SCADA::fill_cell_alignment_lc(unsigned int idx, float* fx, float* fy) {
	SET_VALUES(fx, 0.0F, fy, 0.5F);
}

void WarGrey::SCADA::fill_cell_alignment_cc(unsigned int idx, float* fx, float* fy) {
	SET_VALUES(fx, 0.5F, fy, 0.5F);
}

void WarGrey::SCADA::fill_cell_alignment_rc(unsigned int idx, float* fx, float* fy) {
	SET_VALUES(fx, 1.0F, fy, 0.5F);
}

/*************************************************************************************************/
private struct tcell {
	Platform::String^ value;
	CanvasTextLayout^ content;
	CanvasTextFormat^ font;
};

private class WarGrey::SCADA::TableColumn {
public:
	~TableColumn() noexcept {
		this->clear_pool();
	}

public:
	void update_font(CanvasTextFormat^ head_font, CanvasTextFormat^ font) {
		this->caption = make_text_layout(this->name, head_font);
		this->cell_font = font;
	}

public:
	/** WARNING:
	 * Move cursor only when `this->empty()` returns `false`,
	 * so that there is no need to check the existence of current iterator slot every time.
	 */

	void cursor_end() {
		// NOTE: For reverse iteration, the current slot should greater than or equal to the history slot.
		this->virtual_iterator_slot = this->virtual_current_slot + this->slot_count;
	}

	CanvasTextLayout^ cursor_step_backward() {
		CanvasTextLayout^ value = nullptr;

		if (this->virtual_iterator_slot >= this->virtual_history_slot) {
			long long iterator_slot = this->virtual_iterator_slot % this->slot_count;
		
			this->virtual_iterator_slot -= 1;

			if ((this->cells[iterator_slot].content == nullptr) || (this->cells[iterator_slot].font != this->cell_font)) {
				this->cells[iterator_slot].content = make_text_layout(this->cells[iterator_slot].value, this->cell_font);
				this->cells[iterator_slot].font = this->cell_font;
			}

			value = this->cells[iterator_slot].content;
		}

		return value;
	}

	void push_front_value(Platform::String^ value) {
		long long current_slot = this->virtual_history_slot % this->slot_count;

		if (!this->history_full) {
			if (this->virtual_history_slot > this->virtual_current_slot) {
				this->cells[current_slot].value = value;
				this->cells[current_slot].content = nullptr;
				this->cells[current_slot].font = this->cell_font;
				this->virtual_history_slot -= 1;
			}
		}
	}

	void push_back_value(Platform::String^ value) {
		long long current_slot = this->virtual_current_slot % this->slot_count;

		if (this->virtual_history_slot <= this->virtual_current_slot) {
			this->history_full = true;
			this->virtual_history_slot += 1;
		}

		this->cells[current_slot].value = value;
		this->cells[current_slot].content = nullptr;
		this->virtual_current_slot += 1;
	}

public:
	void reset_pool(long long history_max) {
		this->clear_pool();

		this->slot_count = history_max;

		this->cells = new tcell[this->slot_count];

		this->virtual_current_slot = 0;
		this->virtual_history_slot = this->slot_count - 1;
		this->history_full = false;

		for (long long idx = 0; idx < this->slot_count; idx++) {
			this->cells[idx].value = nullptr;
			this->cells[idx].content = nullptr;
			this->cells[idx].font = table_default_cell_font;
		}

		this->cell_font = table_default_cell_font;
	}

	void clear_pool() {
		if (this->cells != nullptr) {
			delete[] this->cells;
		}
	}

public:
	CanvasTextLayout^ caption;
	Platform::String^ name;
	float width;
	float fx;
	float fy;

private:
	tcell* cells = nullptr;
	long long slot_count = 0;
	long long virtual_current_slot;
	long long virtual_history_slot;
	bool history_full;
	
private:
	long long virtual_iterator_slot;
	CanvasTextFormat^ cell_font;
};

/*************************************************************************************************/
ITablet::ITablet(ITableDataSource* datasrc, float width, float height, unsigned int column_count, long long history_max)
	: IStatelet(TableState::Realtime), width(width), height(height), data_source(datasrc), column_count(column_count)
	, history_max(history_max), request_loading(true) {

	if (this->data_source != nullptr) {
		this->data_source->reference();
	}

	this->enable_events(true);
}

ITablet::~ITablet() {
	if (this->data_source != nullptr) {
		this->data_source->destroy();
	}
	
	if (this->columns != nullptr) {
		delete[] this->columns;
	}
}

void ITablet::update(long long count, long long interval, long long uptime) {
	if (this->request_loading) {
		if (this->data_source != nullptr) {
			if (this->data_source->ready() && (!this->data_source->loading())) {
				this->data_source->load(this, this->history_max);
			}
		} else {
			this->on_maniplation_complete(this->history_max);
		}
	}
}

void ITablet::construct_column(unsigned int idx, Platform::String^ name) {
	TableStyle style = this->get_style();
	
	if (this->width <= 0.0F) {
		this->width = this->available_visible_width();
	}

	if (this->height <= 0.0F) {
		this->height = this->available_visible_height();
	}

	if (this->columns == nullptr) {
		this->columns = new TableColumn[this->column_count];
	}

	this->columns[idx].reset_pool(this->history_max);
	
	if (!name->Equals(this->columns[idx].name)) {
		this->columns[idx].name = name;
	}
}

void ITablet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void ITablet::prepare_style(TableState status, TableStyle& style) {
	CAS_SLOT(style.resolve_column_width_percentage, resolve_average_column_width);
	CAS_SLOT(style.fill_cell_alignment, fill_cell_alignment_cc);

	CAS_SLOT(style.head_font, table_default_head_font);
	CAS_SLOT(style.cell_font, table_default_cell_font);
	CAS_SLOT(style.border_color, table_default_border_color);
	CAS_SLOT(style.head_background_color, table_default_head_background_color);
	CAS_SLOT(style.head_foreground_color, table_default_head_foreground_color);
	CAS_SLOT(style.head_line_color, Colours::WhiteSmoke);
	CAS_SLOT(style.head_line_color, Colours::Transparent);

	CAS_SLOT(style.cell_col_line_style, make_dash_stroke(CanvasDashStyle::Solid));
	CAS_SLOT(style.cell_row_line_style, make_dash_stroke(CanvasDashStyle::Solid));
	CAS_SLOT(style.head_col_line_style, style.cell_col_line_style);
	CAS_SLOT(style.head_row_line_style, style.cell_row_line_style);

	FLCAS_SLOT(style.border_thickness, 1.5F);
	FLCAS_SLOT(style.cell_col_line_thickness, 1.0F);
	FLCAS_SLOT(style.cell_row_line_thickness, 1.0F);
	FLCAS_SLOT(style.head_col_line_thickness, style.cell_col_line_thickness);
	FLCAS_SLOT(style.head_row_line_thickness, style.cell_row_line_thickness);

	FLCAS_SLOT(style.head_minheight_em, 2.000F);
	FLCAS_SLOT(style.cell_height_em, 1.618F);

	FLCAS_SLOT(style.cell_margin, 2.0F);
	FLCAS_SLOT(style.cell_corner_radius, 8.0F);
}

void ITablet::apply_style(TableStyle& style) {
	for (unsigned int idx = 0; idx < this->column_count; idx++) {
		style.fill_cell_alignment(idx, &this->columns[idx].fx, &this->columns[idx].fy);
		
		this->columns[idx].width = style.resolve_column_width_percentage(idx, this->column_count) * this->width;
		this->columns[idx].update_font(style.head_font, style.cell_font);
	}

	{ // resolve row count per page
		float head_height = style.head_font->FontSize * style.head_minheight_em;
		float cell_height = style.cell_font->FontSize * style.cell_height_em;
		
		this->page_row_count = (unsigned int)(std::floorf((this->height - head_height) / cell_height));
	}
}

void ITablet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	bool history = (this->get_state() == TableState::History);
	TableStyle style = this->get_style();
	float border_off = style.border_thickness * 0.5F;
	float cell_height = style.cell_font->FontSize * style.cell_height_em;
	float row0_y = y;
	
	{ // draw table head
		float column_x = x;
		float head_height = this->height - cell_height * float(this->page_row_count);

		row0_y += head_height;

		ds->FillRectangle(x, y, this->width, head_height, style.head_background_color);

		for (unsigned int idx = 0; idx < this->column_count; idx++) {
			TableColumn* col = &this->columns[idx];
			float caption_x = column_x + (col->width - col->caption->LayoutBounds.Width) * 0.5F;
			float caption_y = y + (head_height - col->caption->LayoutBounds.Height) * 0.5F;

			column_x += col->width;
			ds->DrawLine(column_x, y, column_x, row0_y, style.head_line_color, style.head_col_line_thickness, style.head_col_line_style);
			ds->DrawTextLayout(col->caption, caption_x, caption_y, style.head_foreground_color);
		}

		ds->DrawLine(x, row0_y, x + this->width, row0_y, style.head_line_color, style.head_row_line_thickness, style.head_row_line_style);
	}

	if (this->page_row_count > 0) {
		/*
		for (unsigned idx = 0; idx < this->count; idx++) {
			TableColumn* line = &this->lines[idx];
			tsdouble cursor_flonum;

			line->selected_value = std::nanf("not resolved");

			if (!line->hiden) {
				float minimum_diff = style.selected_thickness * 0.5F;
				float tolerance = style.lines_thickness;
				float last_x = std::nanf("no datum");
				float last_y = std::nanf("no datum");
				float rx = x + haxes_box.Width;
				CanvasPathBuilder^ area = nullptr;

				line->cursor_end();

				while (line->cursor_step_backward(&cursor_flonum)) {
					double fx = (double(cursor_flonum.timepoint) - double(ts->start * 1000)) / double(ts->span * 1000);
					double fy = (this->vmin == this->vmax) ? 1.0 : (this->vmax - cursor_flonum.value) / (this->vmax - this->vmin);
					float this_x = x + haxes_box.X + float(fx) * haxes_box.Width;
					float this_y = y + haxes_box.Y + float(fy) * haxes_box.Height;
					float this_diff = std::fabsf(this_x - x_axis_selected);

					if (this_diff < minimum_diff) {
						minimum_diff = this_diff;
						line->y_axis_selected = this_y;
						line->selected_value = cursor_flonum.value;
					}

					if (std::isnan(last_x) || (this_x > rx)) {
						last_x = this_x;
						last_y = this_y;
						x_axis_max = last_x;
					} else {
						if (((last_x - this_x) > tolerance) || (std::fabsf(this_y - last_y) > tolerance) || (x_axis_max == last_x)) {
							if (line->close_color == nullptr) {
								ds->DrawLine(last_x, last_y, this_x, this_y, line->color, style.lines_thickness, style.lines_style);
							} else {
								if (area == nullptr) {
									area = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
									area->BeginFigure(last_x, y_axis_0);
									area->AddLine(last_x, last_y);
									x_axis_max = last_x;
								} else {
									area->AddLine(this_x, this_y);
								}
							}

							last_x = this_x;
							last_y = this_y;
						}

						if (this_x < x) {
							break;
						}
					}
				}

				if (area != nullptr) {
					if (last_x == x_axis_max) {
						area->EndFigure(CanvasFigureLoop::Open);
						ds->DrawLine(last_x, last_y, last_x, y_axis_0, line->color, style.lines_thickness);
					} else {
						area->AddLine(last_x, y_axis_0);
						area->EndFigure(CanvasFigureLoop::Closed);

						{ // draw closed line area
							CanvasGeometry^ garea = CanvasGeometry::CreatePath(area);

							ds->FillGeometry(garea, 0.0F, 0.0F, line->close_color);
							ds->DrawGeometry(garea, 0.0F, 0.0F, line->color, style.lines_thickness);

						}
					}
				}
			}
		}

		ds->DrawCachedGeometry(this->vmarks, x, y, style.vaxes_color);
		ds->DrawCachedGeometry(this->hmarks, x, y, style.haxes_color); */
	}

	ds->DrawRectangle(x + border_off, y + border_off,
		this->width - style.border_thickness, this->height - style.border_thickness,
		style.border_color, style.border_thickness);
}

void ITablet::set_row(Platform::String^ fields[]) {
	TableStyle style = this->get_style();

	for (unsigned int idx = 0; idx < this->column_count; idx++) {
		this->columns[idx].push_back_value(fields[idx]);
	}

	this->notify_updated();
}

void ITablet::on_row_datum(long long request_count, long long nth, Platform::String^ fields[], unsigned int n) {
	if (this->history_max == request_count) {
		for (unsigned int idx = 0; idx < this->column_count; idx++) {
			this->columns[idx].push_front_value(fields[idx]);
		}
	}
}

void ITablet::on_maniplation_complete(long long request_count) {
	if (this->history_max == request_count) {
		this->request_loading = false;
	}
}

void ITablet::set_history_interval(long long open_s, long long close_s, bool force) {
	/*
	long long span = std::max(std::abs(open_s - close_s), this->realtime.span);
	long long destination = std::max(open_s, close_s);

	if (force || (this->history_destination != destination) || (this->history_span != span)) {
		if (this->data_source != nullptr) {
			this->data_source->cancel();
		}

		this->history_span = span;
		this->history_destination = destination;

		this->realtime.start = this->history_destination - this->realtime.span;
		this->loading_timepoint = this->history_destination;
		this->history = this->realtime;

		this->update_horizontal_axes(this->get_style());

		for (unsigned int idx = 0; idx < this->count; idx++) {
			this->construct_line(idx, this->lines[idx].name);
		}

		this->notify_updated();
	}
	*/
}

void ITablet::scroll_to_timepoint(long long timepoint_ms, float proportion) {
	/*
	long long axes_interval = this->realtime.span / this->realtime.step;
	long long inset = (long long)(std::roundf(float(axes_interval) * proportion));
	long long timepoint = timepoint_ms / 1000LL;

	if (timepoint < this->realtime.start + inset) {
		this->update_time_series(timepoint - inset);
		this->notify_updated();
	} else if (timepoint > (this->realtime.start + this->realtime.span - inset)) {
		this->update_time_series(timepoint + inset + axes_interval - this->realtime.span);
		this->notify_updated();
	}

	if (this->get_state() == TableState::Realtime) {
		this->selected_x = float(timepoint - this->realtime.start) / float(this->realtime.span) * this->width;
	}
	*/
}

void ITablet::own_caret(bool yes) {
	this->set_state(yes ? TableState::History : TableState::Realtime);
}

bool ITablet::on_key(VirtualKey key, bool screen_keyboard) {
	/*
	long long limit = ((this->history_destination <= 0) ? current_seconds() : this->history_destination);
	long long interval = (this->history.span >> 3);
	long long start_left_limit = limit - this->history_span;
	long long start_right_limit = limit - interval;
	*/

	bool handled = true;

	/*
	switch (key) {
	case VirtualKey::Left: {
		this->history.start -= interval;
		this->history.start = std::max(this->history.start, start_left_limit);
	}; break;
	case VirtualKey::Right: {
		this->history.start += interval;
		this->history.start = std::min(this->history.start, start_right_limit);
	}; break;
	case VirtualKey::Add: {
		this->history.span = this->history.span >> 1;
		this->history.span = std::max(this->history.span, minute_span_s);
	}; break;
	case VirtualKey::Subtract: {
		this->history.span = this->history.span << 1;
		this->history.span = std::min(this->history.span, day_span_s);
	}; break;
	case VirtualKey::Home: this->history.start = start_left_limit; break;
	case VirtualKey::End: this->history.start = start_right_limit; break;
	case VirtualKey::Escape: this->history = this->realtime; break;
	default: handled = false; break;
	}

	if (handled) {
		this->no_selected();
		this->update_horizontal_axes(this->get_style());
	}
	*/

	return handled;
}
