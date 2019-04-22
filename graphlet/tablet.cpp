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

#define PAGE_COUNT(total, count) int(ceilf(float(total) / float(count)))

static CanvasSolidColorBrush^ table_default_border_color = Colours::make(0xBBBBBB);
static CanvasTextFormat^ table_default_head_font = make_bold_text_format(16.0F);
static CanvasTextFormat^ table_default_cell_font = make_bold_text_format(14.0F);

static CanvasSolidColorBrush^ table_default_background_color = Colours::make(0x414141U);
static CanvasSolidColorBrush^ table_default_head_background_color = Colours::make(0x8FBC8F);
static CanvasSolidColorBrush^ table_default_head_foreground_color = Colours::make(0xF8F8FF);
static CanvasSolidColorBrush^ table_default_cell_background_color = Colours::make(0x141414U);
static CanvasSolidColorBrush^ table_default_cell_foreground_color = Colours::make(0xF8F8FF);

static VirtualKey gestures[] = { VirtualKey::Home, VirtualKey::Down, VirtualKey::Left, VirtualKey::Right, VirtualKey::Up, VirtualKey::End };

static Platform::String^ gesture_symbol(VirtualKey gesture) {
	Platform::String^ symbol = nullptr;
	
	switch (gesture) {
	case VirtualKey::Home: symbol = L"⇤"; break;
	case VirtualKey::Down: symbol = L"↞"; break;
	case VirtualKey::Left: symbol = L"←"; break;
	case VirtualKey::Right: symbol = L"→"; break;
	case VirtualKey::Up: symbol = L"↠"; break;
	case VirtualKey::End: symbol = L"⇥"; break;
	};

	return symbol;
}

/*************************************************************************************************/
float WarGrey::SCADA::resolve_average_column_width(unsigned int idx, unsigned int column_count) {
	return 1.0F / float(column_count);
}

void WarGrey::SCADA::prepare_default_cell_style(unsigned int idx, long long row_identity, TableCellStyle* style) {
	CAS_SLOT(style->background_color, Colours::Background);
	CAS_SLOT(style->foreground_color, Colours::GhostWhite);

	FLCAS_SLOT(style->margin, 2.0F);
	FLCAS_SLOT(style->corner_radius, 4.0F);

	if ((style->align_fx < 0.0F) || (style->align_fx > 1.0F)) {
		style->align_fx = 0.5F;
	}

	if ((style->align_fy < 0.0F) || (style->align_fy > 1.0F)) {
		style->align_fy = 0.5F;
	}
}

/*************************************************************************************************/
private struct theader {
	CanvasTextLayout^ caption;
	Platform::String^ name;
	float width;
};

private struct tcell {
	Platform::String^ value;
	CanvasTextLayout^ content;
	TableCellStyle style;
	bool outdated;
};

private struct trow {
	long long salt;
	CanvasTextFormat^ font;
	tcell* cells; // managed by TableBeing
};

private class WarGrey::SCADA::TableBeing {
public:
	~TableBeing() noexcept {
		this->clear_pool();

		delete[] this->headers;
	}

	TableBeing(Platform::String^ columns[], unsigned int count) : column_count(count) {
		this->headers = new theader[count];

		for (unsigned int idx = 0; idx < count; idx++) {
			this->headers[idx].name = columns[idx];
		}
	}

public:
	void update_font(unsigned int idx, CanvasTextFormat^ head_font, CanvasTextFormat^ cell_font, CanvasTextFormat^ status_font) {
		this->headers[idx].caption = make_text_layout(this->headers[idx].name, head_font);
		this->cell_font = cell_font;

		for (size_t idx = 0; idx < sizeof(gestures) / sizeof(VirtualKey); idx++) {
			this->gesture_captions[idx] = make_text_layout(gesture_symbol(gestures[idx]), status_font);
		}
	}

public:
	trow* tail_ref(unsigned int idx) {
		long long target_slot = (this->virtual_current_slot + this->slot_count - idx - 1) % this->slot_count;
		trow* row = nullptr;

		if (this->rows[target_slot].cells != nullptr) {
			row = &this->rows[target_slot];

			for (unsigned int idx = 0; idx < this->column_count; idx++) {
				if ((row->cells[idx].content == nullptr) || (row->font != this->cell_font)) {
					row->cells[idx].content = make_text_layout(row->cells[idx].value, this->cell_font);
					row->cells[idx].outdated = true;
					row->font = this->cell_font;
				}
			}
		}
	
		return row;
	}

	void update_row(long long salt, Platform::String^ fields[]) {
		for (long long idx = this->virtual_history_slot; idx <= this->virtual_current_slot + this->slot_count; idx++) {
			long long current_slot = idx % this->slot_count;

			if (this->rows[current_slot].cells != nullptr) {
				if (this->rows[current_slot].salt == salt) {
					for (unsigned int idx = 0; idx < this->column_count; idx++) {
						this->rows[current_slot].cells[idx].value = fields[idx];
						this->rows[current_slot].cells[idx].content = nullptr;
					}

					break;
				}
			}
		}
	}

	void push_front_row(long long salt, Platform::String^ fields[]) {
		long long current_slot = this->virtual_history_slot % this->slot_count;

		if (!this->history_full) {
			if (this->virtual_history_slot > this->virtual_current_slot) {
				if (this->rows[current_slot].cells == nullptr) {
					this->rows[current_slot].cells = new tcell[this->column_count];
				}

				this->rows[current_slot].salt = salt;
				this->rows[current_slot].font = this->cell_font;

				for (unsigned int idx = 0; idx < this->column_count; idx++) {
					this->rows[current_slot].cells[idx].value = fields[idx];
					this->rows[current_slot].cells[idx].content = nullptr;
					this->rows[current_slot].cells[idx].outdated = true;
				}

				this->virtual_history_slot -= 1;
			}
		}
	}

	void push_back_row(long long salt, Platform::String^ fields[]) {
		long long current_slot = this->virtual_current_slot % this->slot_count;

		if (this->virtual_history_slot <= this->virtual_current_slot) {
			this->history_full = true;
			this->virtual_history_slot += 1;
		}

		if (this->rows[current_slot].cells == nullptr) {
			this->rows[current_slot].cells = new tcell[this->column_count];
		}

		this->rows[current_slot].salt = salt;
		this->rows[current_slot].font = this->cell_font;

		for (unsigned int idx = 0; idx < this->column_count; idx++) {
			this->rows[current_slot].cells[idx].value = fields[idx];
			this->rows[current_slot].cells[idx].content = nullptr;
			this->rows[current_slot].cells[idx].outdated = true;
		}
		
		this->virtual_current_slot += 1;
	}

public:
	unsigned long long size() {
		return (unsigned long long)(this->virtual_current_slot + this->slot_count - this->virtual_history_slot - 1LL);
	}

public:
	void reset_pool(long long history_max) {
		this->clear_pool();

		this->slot_count = (unsigned int)(history_max);

		this->rows = new trow[this->slot_count];

		this->virtual_current_slot = 0;
		this->virtual_history_slot = this->slot_count - 1;
		this->history_full = false;

		for (long long idx = 0; idx < this->slot_count; idx++) {
			this->rows[idx].cells = nullptr;
		}

		this->cell_font = table_default_cell_font;
	}

	void clear_pool() {
		if (this->rows != nullptr) {
			for (unsigned int idx = 0; idx < this->slot_count; idx++) {
				if (this->rows[idx].cells != nullptr) {
					delete[] this->rows[idx].cells;
				}
			}

			delete[] this->rows;
		}
	}

public:
	CanvasTextLayout^ gesture_captions[sizeof(gestures) / sizeof(VirtualKey)];
	unsigned int column_count;
	theader* headers;

private:
	trow* rows = nullptr;
	unsigned int slot_count = 0;
	long long virtual_current_slot;
	long long virtual_history_slot;
	bool history_full;
	
private:
	CanvasTextFormat^ cell_font;
};

/*************************************************************************************************/
ITablet::ITablet(ITableDataSource* datasrc, float width, float height, long long history_max)
	: IStatelet(TableState::Realtime), width(width), height(height), page_index(0LL)
	, filter(nullptr), on_description(nullptr), off_description(nullptr), filter_actived(false)
	, data_source(datasrc), history_max(history_max), request_loading(true) {

	if (this->data_source != nullptr) {
		this->data_source->reference();
	}

	this->enable_events(true);
}

ITablet::~ITablet() {
	if (this->data_source != nullptr) {
		this->data_source->destroy();
	}
	
	if (this->table != nullptr) {
		delete this->table;
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

void ITablet::construct_table(Platform::String^ fields[], unsigned int count) {
	TableStyle style = this->get_style();
	
	if (this->width <= 0.0F) {
		this->width = this->available_visible_width();
	}

	if (this->height <= 0.0F) {
		this->height = this->available_visible_height();
	}

	if (this->table == nullptr) {
		this->table = new TableBeing(fields, count);
	}

	this->table->reset_pool(this->history_max);
}

void ITablet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void ITablet::prepare_style(TableState status, TableStyle& style) {
	CAS_SLOT(style.resolve_column_width_percentage, resolve_average_column_width);
	CAS_SLOT(style.prepare_cell_style, prepare_default_cell_style);

	CAS_SLOT(style.head_font, table_default_head_font);
	CAS_SLOT(style.cell_font, table_default_cell_font);
	CAS_SLOT(style.status_font, style.cell_font);

	CAS_SLOT(style.border_color, table_default_border_color);
	CAS_SLOT(style.background_color, table_default_background_color);
	CAS_SLOT(style.head_background_color, table_default_head_background_color);
	CAS_SLOT(style.head_foreground_color, table_default_head_foreground_color);
	CAS_SLOT(style.cell_background_color, table_default_cell_background_color);
	CAS_SLOT(style.cell_foreground_color, table_default_cell_foreground_color);
	CAS_SLOT(style.status_foreground_color, style.cell_foreground_color);
	CAS_SLOT(style.status_foreground_hicolor, style.status_foreground_color);
	CAS_SLOT(style.status_background_color, Colours::Transparent);
	CAS_SLOT(style.status_background_hicolor, Colours::RoyalBlue);
	CAS_SLOT(style.head_line_color, Colours::WhiteSmoke);
	CAS_SLOT(style.cell_line_color, Colours::Transparent);
	CAS_SLOT(style.status_line_color, style.head_line_color);
	CAS_SLOT(style.status_info_color, style.status_line_color);
	CAS_SLOT(style.status_border_color, Colours::DodgerBlue);

	CAS_SLOT(style.cell_col_line_style, make_dash_stroke(CanvasDashStyle::Solid));
	CAS_SLOT(style.cell_row_line_style, make_dash_stroke(CanvasDashStyle::Solid));
	CAS_SLOT(style.head_col_line_style, style.cell_col_line_style);
	CAS_SLOT(style.head_row_line_style, style.cell_row_line_style);
	CAS_SLOT(style.status_line_style, style.head_row_line_style);

	FLCAS_SLOT(style.border_thickness, 1.5F);
	FLCAS_SLOT(style.cell_col_line_thickness, 1.0F);
	FLCAS_SLOT(style.cell_row_line_thickness, 1.0F);
	FLCAS_SLOT(style.head_col_line_thickness, style.cell_col_line_thickness);
	FLCAS_SLOT(style.head_row_line_thickness, style.cell_row_line_thickness);
	FLCAS_SLOT(style.status_line_thickness, style.head_row_line_thickness);

	FLCAS_SLOT(style.head_minheight_em, 2.4F);
	FLCAS_SLOT(style.cell_height_em, 2.0F);
	FLCAS_SLOT(style.status_height_em, 2.4F);

	FLCAS_SLOT(style.cell_margin, 2.0F);
	FLCAS_SLOT(style.cell_corner_radius, 8.0F);
	FLCAS_SLOT(style.status_margin, style.cell_margin);
	FLCAS_SLOT(style.status_corner_radius, 4.0F);
}

void ITablet::prepare_cell_style(TableStyle& table_style, TableCellStyle& cell_style) {
	CAS_SLOT(cell_style.background_color, table_style.cell_background_color);
	CAS_SLOT(cell_style.foreground_color, table_style.cell_foreground_color);

	FLCAS_SLOT(cell_style.margin, table_style.cell_margin);
	FLCAS_SLOT(cell_style.corner_radius, table_style.cell_corner_radius);

	FLCAS_SLOT(cell_style.align_fx, 0.5F);
	FLCAS_SLOT(cell_style.align_fy, 0.5F);
}

void ITablet::apply_style(TableStyle& style) {
	float available_width = this->width - style.border_thickness * 2.0F;
	float available_height = this->height - style.border_thickness * 2.0F;

	for (unsigned int idx = 0; idx < this->table->column_count; idx++) {
		this->table->headers[idx].width = style.resolve_column_width_percentage(idx, this->table->column_count) * available_width;
		this->table->update_font(idx, style.head_font, style.cell_font, style.status_font);
	}

	this->update_description(style);

	{ // resolve row count per page
		float head_height = style.head_font->FontSize * style.head_minheight_em;
		float cell_height = style.cell_font->FontSize * style.cell_height_em;
		float status_height = style.status_font->FontSize * style.status_height_em;
		
		this->page_row_count = (unsigned int)(floorf((available_height - head_height - status_height) / cell_height));
		this->update_statistics(style);
	}
}

void ITablet::update_statistics(TableStyle& style) {
	Platform::String^ description = make_wstring(L"%ld / %ld", this->page_index + 1, PAGE_COUNT(this->history_max, this->page_row_count));

	this->statistics = make_text_layout(description, style.status_font);
}

void ITablet::update_description(TableStyle& style) {
	this->active_description = make_text_layout(this->on_description, style.status_font);

	if (this->off_description == nullptr) {
		this->inactive_description = this->active_description;
	} else {
		this->inactive_description = make_text_layout(this->off_description, style.status_font);
	}
}

void ITablet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	bool history = (this->get_state() == TableState::History);
	TableStyle style = this->get_style();
	float border_off = style.border_thickness * 0.5F;
	float cell_height = style.cell_font->FontSize * style.cell_height_em;
	float status_height = style.status_font->FontSize * style.status_height_em;
	float yrow = y; // no `+ border_off`;
	float ymax = y + this->height - border_off - status_height;
	unsigned long long total = this->count();
	
	ds->FillRectangle(x, y, this->width, this->height, style.background_color);

	{ // draw table header (and cell column lines)
		float xcol = x + border_off * 2.0F;
		float head_height = this->height - style.border_thickness * 2.0F - cell_height * float(this->page_row_count) - status_height;

		yrow += head_height;

		ds->FillRectangle(x, y, this->width, head_height, style.head_background_color);

		for (unsigned int idx = 0; idx < this->table->column_count; idx++) {
			theader* col = &this->table->headers[idx];
			float caption_x = xcol + (col->width - col->caption->LayoutBounds.Width) * 0.5F;
			float caption_y = y + (head_height - col->caption->LayoutBounds.Height) * 0.5F;

			xcol += col->width;
			ds->DrawLine(xcol, y, xcol, yrow, style.head_line_color, style.head_col_line_thickness, style.head_col_line_style);
			ds->DrawLine(xcol, yrow, xcol, ymax, style.cell_line_color, style.cell_col_line_thickness, style.cell_col_line_style);
			ds->DrawTextLayout(col->caption, caption_x, caption_y, style.head_foreground_color);
		}

		ds->DrawLine(x, yrow, x + this->width, yrow, style.head_line_color, style.head_row_line_thickness, style.head_row_line_style);
	}

	{ // draw table body
		unsigned int n_start = this->page_row_count * this->page_index;
		unsigned int ridx = 0U;
		unsigned int n = 0U;

		yrow += style.border_thickness;
		while ((ridx < total) && (n < this->page_row_count)) {
			trow* row = this->table->tail_ref(ridx);
			float xcol = x + border_off * 2.0F;

			if (row != nullptr) {
				if ((n >= n_start) && ((!this->filter_actived) || (this->filter == nullptr) || (this->filter->filter(row->salt)))) {
					for (unsigned int cidx = 0; cidx < this->table->column_count; cidx++) {
						if (row->cells[cidx].outdated) {
							TableCellStyle cell_style;

							// NOTE: do not use `row->cells[cidx].style` directly since it may hold dirty data.
							style.prepare_cell_style(cidx, row->salt, &cell_style);
							this->prepare_cell_style(style, cell_style);

							row->cells[cidx].style = cell_style;
						}
					}

					for (unsigned int cidx = 0; cidx < this->table->column_count; cidx++) {
						CanvasTextLayout^ content = row->cells[cidx].content;
						TableCellStyle* cell_style = &row->cells[cidx].style;
						theader* col = &this->table->headers[cidx];
						float margin = row->cells[cidx].style.margin;
						float cradius = row->cells[cidx].style.corner_radius;
						float bgx = xcol + margin;
						float bgy = yrow + margin;
						float bgwidth = col->width - margin * 2.0F;
						float bgheight = cell_height - margin * 2.0F;
						float content_x = bgx + cradius + (bgwidth - cradius * 2.0F - content->LayoutBounds.Width) * cell_style->align_fx;
						float content_y = bgy + (bgheight - content->LayoutBounds.Height) * cell_style->align_fy;

						ds->FillRoundedRectangle(bgx, bgy, bgwidth, bgheight, cradius, cradius, cell_style->background_color);
						ds->DrawTextLayout(content, content_x, content_y, cell_style->foreground_color);

						xcol += col->width;
					}

					n++;
					yrow += cell_height;
					ds->DrawLine(x, yrow, x + this->width, yrow, style.cell_line_color, style.cell_row_line_thickness, style.cell_row_line_style);
				}
			}

			ridx++;
		}
	}

	if (status_height > 0.0F) { // draw status
		float status_lx = x + border_off * 2.0F;
		float bradius = style.status_corner_radius;
		float box_size = status_height - style.status_margin * 2.0F;
		float box_y = ymax + style.status_margin;
		float step = box_size + style.status_margin * 2.0F;
		
		ds->DrawLine(x, ymax, x + this->width, ymax, style.status_line_color, style.status_line_thickness, style.status_line_style);

		{ // draw statistics
			Rect sbox = this->statistics->LayoutBounds;
			float stats_cx = x + this->width * 0.5F;
			float stats_cy = box_y + box_size * 0.5F;
			
			ds->DrawTextLayout(this->statistics,
				stats_cx - sbox.Width * 0.5F, stats_cy - sbox.Height * 0.5F,
				style.status_info_color);
		}
		
		for (size_t idx = 0; idx < sizeof(gestures) / sizeof(VirtualKey); idx++) {
			CanvasTextLayout^ gesture = this->table->gesture_captions[idx];
			Rect symbox = gesture->LayoutBounds;
			float box_x = status_lx + style.status_margin + step * float(idx);
			float sym_xoff = (box_size - symbox.Width) * 0.5F;
			float sym_yoff = (box_size - symbox.Height) * 0.5F;

			ds->FillRoundedRectangle(box_x, box_y, box_size, box_size, bradius, bradius, style.status_background_color);
			ds->DrawRoundedRectangle(box_x, box_y, box_size, box_size, bradius, bradius, style.status_border_color);
			ds->DrawTextLayout(gesture, box_x + sym_xoff, box_y + sym_yoff, style.status_foreground_color);
		}

		if (this->filter != nullptr) {
			CanvasTextLayout^ description = (this->filter_actived ? this->active_description : this->inactive_description);
			ICanvasBrush^ fgcolor = (this->filter_actived ? style.status_foreground_hicolor : style.status_foreground_color);
			ICanvasBrush^ bgcolor = (this->filter_actived ? style.status_background_hicolor : style.status_background_color);
			Rect fbox = description->LayoutBounds;
			float filter_width = this->resolve_filter_width();
			float filter_rx = x + this->width - (status_lx - x) - style.status_margin;
			float filter_lx = filter_rx - filter_width;
			float desc_xoff = (filter_width - fbox.Width) * 0.5F;
			float desc_yoff = (box_size - fbox.Height) * 0.5F;

			ds->FillRoundedRectangle(filter_lx, box_y, filter_width, box_size, bradius, bradius, bgcolor);
			ds->DrawRoundedRectangle(filter_lx, box_y, filter_width, box_size, bradius, bradius, style.status_border_color);
			ds->DrawTextLayout(description, filter_lx + desc_xoff, box_y + desc_yoff, fgcolor);
		}
	}

	ds->DrawRectangle(x + border_off, y + border_off,
		this->width - style.border_thickness, this->height - style.border_thickness,
		style.border_color, style.border_thickness);
}

unsigned long long ITablet::count() {
	return this->table->size();
}

void ITablet::push_row(long long salt, Platform::String^ fields[]) {
	this->table->push_back_row(salt, fields);
	this->notify_updated();
}

void ITablet::update_row(long long salt, Platform::String^ fields[]) {
	this->table->update_row(salt, fields);
	this->notify_updated();
}

float ITablet::resolve_filter_width() {
	TableStyle style = this->get_style();
	Rect adbox = this->active_description->LayoutBounds;
	Rect idbox = this->inactive_description->LayoutBounds;
	
	return fmaxf(adbox.Width, idbox.Width) + (style.status_corner_radius + style.status_margin) * 2.0F;
}

void ITablet::set_filter(ITableFilter* filter, Platform::String^ on_description, Platform::String^ off_description) {
	this->filter = filter;
	this->on_description = on_description;
	this->off_description = off_description;

	this->update_description(this->get_style());

	if (this->filter_actived) {
		this->notify_updated();
	}
}

void ITablet::enable_filter(bool on) {
	this->filter_actived = on;
	this->notify_updated();
}

void ITablet::on_row_datum(long long request_count, long long nth, long long salt, Platform::String^ fields[], unsigned int n) {
	if (this->history_max == request_count) {
		this->table->push_front_row(salt, fields);
		this->notify_updated();
	}
}

void ITablet::on_maniplation_complete(long long request_count) {
	if (this->history_max == request_count) {
		this->request_loading = false;
	}
}

void ITablet::own_caret(bool yes) {
	this->set_state(yes ? TableState::History : TableState::Realtime);
}

void ITablet::on_tap(float x, float y) {
	TableStyle style = this->get_style();
	float status_height = style.status_font->FontSize * style.status_height_em;
	float box_size = status_height - style.status_margin * 2.0F;
	float box_y = this->height - status_height + style.status_margin;
	float step = box_size + style.status_margin * 2.0F;
	float inset = style.border_thickness + style.status_margin;
	float filter_rx = this->width - inset;
	float filter_lx = filter_rx - this->resolve_filter_width();

	if ((y >= box_y) && (y <= (box_y + box_size))) { // draw status
		if ((x >= filter_lx) && (x <= filter_rx)) {
			this->filter_actived = !this->filter_actived;
			this->notify_updated();
		} else {
			for (size_t idx = 0; idx < sizeof(gestures) / sizeof(VirtualKey); idx++) {
				float box_x = inset + step * float(idx);

				if ((x >= box_x) && (x <= (box_x + box_size))) {
					this->on_key(gestures[idx], false);
					break;
				}
			}
		}
	}
}

bool ITablet::on_key(VirtualKey key, bool screen_keyboard) {
	int max_index = PAGE_COUNT(this->history_max, this->page_row_count) - 1U;
	unsigned int prev_index = this->page_index;
	int next_index = this->page_index;

	switch (key) {
	case VirtualKey::Home: next_index = 0; break;
	case VirtualKey::Up: next_index += 10; break;
	case VirtualKey::Left: next_index -= 1; break;
	case VirtualKey::Right: next_index += 1; break;
	case VirtualKey::Down: next_index -= 10; break;
	case VirtualKey::End: next_index = max_index; break;
	}

	if (next_index <= 0) {
		this->page_index = 0U;
	} else if (next_index >= max_index) {
		this->page_index = (unsigned int)max_index;
	} else {
		this->page_index = (unsigned int)next_index;
	}

	if (this->page_index != prev_index) {
		this->update_statistics(this->get_style());
		this->notify_updated();
	}

	return (this->page_index != prev_index);
}
