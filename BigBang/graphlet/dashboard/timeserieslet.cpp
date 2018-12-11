#include "graphlet/dashboard/timeserieslet.hpp"

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

static CanvasSolidColorBrush^ lines_default_border_color = Colours::make(0xBBBBBB);
static CanvasTextFormat^ lines_default_font = make_bold_text_format(12.0F);
static CanvasTextFormat^ lines_default_legend_font = make_bold_text_format(14.0F);

private struct WarGrey::SCADA::TimeSeriesLine {
public:
	void push_front_value(long long timestamp, double value) {
		this->timestamps.push_front(timestamp);
		this->values.push_front(value);
	}

	void push_back_value(long long timestamp, double value) {
		this->timestamps.push_back(timestamp);
		this->values.push_back(value);
	}

	void update_legend(unsigned int precision, WarGrey::SCADA::TimeSeriesStyle& style, CanvasSolidColorBrush^ color = nullptr) {
		Platform::String^ legend = this->name + ": ";

		legend += (this->values.empty() ? speak("nodatum", "status") : flstring(this->values.back(), precision));
		this->legend = make_text_layout(legend, style.legend_font);

		if (color != nullptr) {
			this->color = color;
		}
	}

public:
	std::deque<long long> timestamps;
	std::deque<double> values;
	double selected_value;
	float y_axis_selected;

public:
	Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ color;
	Microsoft::Graphics::Canvas::Brushes::CanvasSolidColorBrush^ close_color;
	Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ legend;
	Platform::String^ name;
	bool hiden;
};

/*************************************************************************************************/
TimeSeries WarGrey::SCADA::make_minute_series(unsigned int count, unsigned int step) {
	TimeSeries ts;

	ts.span = minute_span_s * std::max(count, 1U);
	ts.start = current_floor_seconds(minute_span_s) - ts.span / 2;
	ts.step = step;

	return ts;
}

TimeSeries WarGrey::SCADA::make_hour_series(unsigned int count, unsigned int step) {
	TimeSeries ts;

	ts.span = hour_span_s * std::max(count, 1U);
	ts.start = current_floor_seconds(hour_span_s) - ts.span / 2;
	ts.step = step;

	return ts;
}

TimeSeries WarGrey::SCADA::make_today_series(unsigned int step) {
	TimeSeries ts;

	ts.span = day_span_s;
	ts.start = current_floor_seconds(ts.span);
	ts.step = step;

	return ts;
}

CanvasSolidColorBrush^ WarGrey::SCADA::lookup_default_light_color(unsigned int idx) {
	return make_solid_brush(lookup_light_color(idx + 1));
}

CanvasSolidColorBrush^ WarGrey::SCADA::lookup_default_dark_color(unsigned int idx) {
	return make_solid_brush(lookup_dark_color(idx + 1));
}

/*************************************************************************************************/
ITimeSerieslet::ITimeSerieslet(ITimeSeriesDataSource* datasrc
	, double vmin, double vmax, TimeSeries& ts, unsigned int n, float width, float height
	, unsigned int step, unsigned int precision, long long history_max)
	: IStatelet(TimeSeriesState::Realtime), width(std::fabsf(width)), height(height), precision(precision)
	, data_source(datasrc), vmin(vmin), vmax(vmax), count(n), vertical_step((step == 0) ? 5U : step)
	, realtime(ts), history(ts), history_max(history_max), selected_x(std::nanf("not exists")) {

	if (this->height == 0.0F) {
		this->height = this->width * 0.2718F;
	}

	if (this->vmin > this->vmax) {
		this->vmin = vmax;
		this->vmax = vmin;
	}

	if (this->data_source != nullptr) {
		this->data_source->reference();
	}

	this->enable_events(true);
	this->next_loading_timepoint = current_seconds();
}

ITimeSerieslet::~ITimeSerieslet() {
	if (this->lines != nullptr) {
		delete[] this->lines;
	}

	if (this->data_source != nullptr) {
		this->data_source->destroy();
	}
}

void ITimeSerieslet::update(long long count, long long interval, long long uptime) {
	long long axes_interval = this->realtime.span / this->realtime.step;
	long long boundary = this->realtime.start + this->realtime.span - (axes_interval * 3 / 2);
	long long now = current_seconds();

	if (now > boundary) {
		this->update_time_series(this->realtime.start + axes_interval);
		this->notify_updated();
	}

	{ // load exists data
		TimeSeries* ts = ((this->get_state() == TimeSeriesState::History) ? &this->history : &this->realtime);
		long long exists_earliest_s = this->next_loading_timepoint;
		long long request_earliest_s = std::min(ts->start, now - this->history_max);
		long long request_interval = this->history_max / this->realtime.step;
		
		if (exists_earliest_s > request_earliest_s) {
			if ((this->data_source != nullptr) && this->data_source->ready() && (!this->data_source->loading())) {
				this->data_source->load(this, exists_earliest_s, (exists_earliest_s - request_interval));
			}
		}
	}
}

void ITimeSerieslet::construct_line(unsigned int idx, Platform::String^ name) {
	TimeSeriesStyle style = this->get_style();

	if (this->lines == nullptr) {
		this->lines = new TimeSeriesLine[this->count];
	}

	this->lines[idx].name = name;
	this->lines[idx].close_color = nullptr;
	this->lines[idx].hiden = false;
}

void ITimeSerieslet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void ITimeSerieslet::prepare_style(TimeSeriesState status, TimeSeriesStyle& style) {
	CAS_SLOT(style.lookup_color, lookup_default_light_color);

	CAS_SLOT(style.font, lines_default_font);
	CAS_SLOT(style.legend_font, lines_default_legend_font);
	CAS_SLOT(style.border_color, lines_default_border_color);
	CAS_SLOT(style.haxes_color, Colours::DodgerBlue);
	CAS_SLOT(style.haxes_style, make_dash_stroke(CanvasDashStyle::DashDot));
	CAS_SLOT(style.vaxes_color, Colours::Tomato);
	CAS_SLOT(style.vaxes_style, make_dash_stroke(CanvasDashStyle::DashDot));
	CAS_SLOT(style.selected_color, Colours::GhostWhite);
	CAS_SLOT(style.selected_style, make_dash_stroke(CanvasDashStyle::DashDotDot));
	CAS_SLOT(style.lines_style, make_roundcap_stroke_style());

	FLCAS_SLOT(style.border_thickness, 1.5F);
	FLCAS_SLOT(style.lines_thickness, 1.0F);
	FLCAS_SLOT(style.haxes_thickness, 0.5F);
	FLCAS_SLOT(style.vaxes_thickness, 0.5F);
	FLCAS_SLOT(style.selected_thickness, 1.0F);

	if ((style.legend_fx < 0.0F) || (style.legend_fx > 1.0F)) {
		style.legend_fx = 0.81F;
	}
}

void ITimeSerieslet::on_state_changed(TimeSeriesState status) {
	TimeSeriesStyle style = this->get_style();

	this->update_vertical_axes(style);
	this->update_horizontal_axes(style);

	for (unsigned int idx = 0; idx < this->count; idx++) {
		this->lines[idx].update_legend(this->precision + 1U, style, style.lookup_color(idx));
	}
}

void ITimeSerieslet::update_time_series(long long next_start) {
	if (this->history.start >= this->realtime.start) {
		this->history.start = next_start;
	}

	this->realtime.start = next_start;
	this->update_horizontal_axes(this->get_style());

	{ // TODO: remove old data
		long long earliest_s = this->realtime.start - this->history_max;

		this->begin_maniplation_sequence();

		for (unsigned int idx = 0; idx < this->count; idx++) {
			TimeSeriesLine* line = &this->lines[idx];
			unsigned int count = 0;
			bool done = true;
			
			do {
				done = true;

				if (!line->timestamps.empty()) {
					long long front_s = line->timestamps.front() / 1000L;

					if (front_s < earliest_s) {
						line->timestamps.pop_front();
						line->values.pop_front();
						done = false;

						count++;
					}
				}
			} while (!done);
		}

		this->end_maniplation_sequence();

		this->get_logger()->log_message(Log::Info,
			L"removed %d data@%s",
			count, update_nowstamp(true)->Data());
	}
}

void ITimeSerieslet::update_vertical_axes(TimeSeriesStyle& style) {
	CanvasGeometry^ vaxes = blank();
	CanvasGeometry^ marks = blank();
	CanvasPathBuilder^ axes = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float interval = this->height / float(this->vertical_step + 1);
	double delta = (this->vmax - this->vmin) / double(this->vertical_step + 1);
	float y = this->height - style.haxes_thickness * 0.5F;
	TextExtent mark_te;

	for (unsigned int i = 1; i <= vertical_step; i++) {
		float ythis = y - interval * float(i);
		Platform::String^ mark = flstring(this->vmin + delta * double(i), this->precision);
		CanvasGeometry^ gmark = paragraph(mark, style.font, &mark_te);

		marks = geometry_union(marks, gmark, style.border_thickness + mark_te.height * 0.618F, ythis - mark_te.height);

		axes->BeginFigure(0.0F, ythis);
		axes->AddLine(this->width, ythis);
		axes->EndFigure(CanvasFigureLoop::Open);
	}

	this->vmarks = geometry_freeze(marks);
	this->vaxes = geometry_freeze(geometry_stroke(CanvasGeometry::CreatePath(axes), style.vaxes_thickness, style.vaxes_style));
}

void ITimeSerieslet::update_horizontal_axes(TimeSeriesStyle& style) {
	TimeSeries* ts = ((this->get_state() == TimeSeriesState::History) ? &this->history : &this->realtime);
	CanvasPathBuilder^ axes = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	CanvasGeometry^ hmarks = blank();
	float interval = this->width / float(ts->step + 1);
	long long delta = ts->span / (ts->step + 1);
	float x = style.haxes_thickness * 0.5F;
	float y = this->height - style.border_thickness;
	TextExtent date_mark_te, time_mark_te;

	for (unsigned int i = 0; i <= ts->step + 1; i++) {
		float xthis = x + interval * float(i);
		long long utc_s = ts->start + delta * i;
		Platform::String^ date_mark = make_datestamp_utc(utc_s, true);
		Platform::String^ time_mark = make_daytimestamp_utc(utc_s, true);
		CanvasGeometry^ gdatemark = paragraph(date_mark, style.font, &date_mark_te);
		CanvasGeometry^ gtimemark = paragraph(time_mark, style.font, &time_mark_te);

		axes->BeginFigure(xthis, 0.0F);
		axes->AddLine(xthis, this->height);
		axes->EndFigure(CanvasFigureLoop::Open);

		hmarks = geometry_union(hmarks, gtimemark, xthis - time_mark_te.width * 0.5F, y - time_mark_te.height);
		hmarks = geometry_union(hmarks, gdatemark, xthis - date_mark_te.width * 0.5F, y - date_mark_te.height - time_mark_te.height);
	}

	this->hmarks = geometry_freeze(hmarks);
	this->haxes = geometry_stroke(CanvasGeometry::CreatePath(axes), style.haxes_thickness, style.haxes_style);
}

void ITimeSerieslet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	bool history = (this->get_state() == TimeSeriesState::History);
	TimeSeries* ts = (history ? &this->history : &this->realtime);
	TimeSeriesStyle style = this->get_style();
	Rect haxes_box = this->haxes->ComputeBounds();
	float x_axis_selected = x + this->selected_x;
	float border_off = style.border_thickness * 0.5F;
	float x_axis_max = std::nanf("unknown");
	float y_axis_max = y + haxes_box.Y;
	
	/** WARNING
	 * It seems that Win2D/Direct2D Path object does not like overlaid lines,
	 * thus, it is error-prone to close the line with x-axis.
	 * just in case, `style.lines_thickness * 2.0F` is the adjustment.
	 */
	float y_axis_0 = y_axis_max + haxes_box.Height + style.lines_thickness * 2.0F;
	
	ds->FillRectangle(x, y, this->width, this->height, Colours::Background);
	ds->DrawCachedGeometry(this->vaxes, x, y, style.vaxes_color);
	ds->FillGeometry(this->haxes, x, y, style.haxes_color);

	{ // draw legends
		float legend_x = x + this->width * style.legend_fx;
		float legend_label_height = this->lines[0].legend->LayoutBounds.Height;
		float legend_label_x = legend_x + legend_label_height * 1.618F;
		float legend_width = legend_label_height;
		float legend_height = legend_label_height * 0.618F;
		float legend_yoff = (legend_label_height - legend_height) * 0.5F;
		float flcount = 0.0F;

		for (unsigned int idx = 0; idx < this->count; idx++) {
			TimeSeriesLine* line = &this->lines[idx];

			if (!line->hiden) {
				float yoff = legend_label_height * (flcount + 0.618F);

				ds->FillRectangle(legend_x, y + legend_yoff + yoff, legend_width, legend_height, line->color);
				ds->DrawTextLayout(line->legend, legend_label_x, y + yoff, line->color);

				flcount += 1.0F;
			}
		}
	}

	for (unsigned idx = 0; idx < this->count; idx++) {
		TimeSeriesLine* line = &this->lines[idx];

		line->selected_value = std::nanf("not resolved");

		if (!line->hiden) {
			float last_x = std::nanf("no datum");
			float last_y = std::nanf("no datum");
			float tolerance = style.lines_thickness;
			float rx = x + haxes_box.Width;
			auto t = line->timestamps.rbegin();
			auto v = line->values.rbegin();
			auto end = line->timestamps.rend();
			CanvasPathBuilder^ area = nullptr;
			float minimum_diff = style.selected_thickness * 0.5F;

			while (t != end) {
				double fx = (double(*t) - double(ts->start * 1000)) / double(ts->span * 1000);
				double fy = (this->vmin == this->vmax) ? 1.0 : (this->vmax - (*v)) / (this->vmax - this->vmin);
				float this_x = x + haxes_box.X + float(fx) * haxes_box.Width;
				float this_y = y + haxes_box.Y + float(fy) * haxes_box.Height;
				float this_diff = std::fabsf(this_x - x_axis_selected);

				if (this_diff < minimum_diff) {
					minimum_diff = this_diff;
					line->y_axis_selected = this_y;
					line->selected_value = (*v);
				}

				if (std::isnan(last_x) || (this_x > rx)) {
					last_x = this_x;
					last_y = this_y;
					x_axis_max = last_x;
				} else {
					if (((last_x - this_x) > tolerance) || (std::fabsf(this_y - last_y) > tolerance) || (x_axis_max == last_x)) {
						if (line->close_color == nullptr) {
							ds->DrawLine(last_x, last_y, this_x, this_y, this->lines[idx].color,
								style.lines_thickness, style.lines_style);
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

				t++;
				v++;
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
	ds->DrawCachedGeometry(this->hmarks, x, y, style.haxes_color);

	if ((history) && (x_axis_selected > x)) {
		float last_xoff = 0.0F;
		float last_y = y + this->height;
		
		ds->DrawLine(x_axis_selected, y_axis_0, x_axis_selected, y_axis_max,
			style.selected_color, style.selected_thickness, style.selected_style);

		for (unsigned idx = 0; idx < this->count; idx++) {
			TimeSeriesLine* line = &this->lines[idx];

			if (!std::isnan(line->selected_value)) {
				Platform::String^ metric = line->name + ": " + flstring(line->selected_value, this->precision);
				CanvasTextLayout^ desc = make_text_layout(metric, style.legend_font);
				Rect this_box = desc->LayoutBounds;
				float yoff = desc->LayoutBounds.Height;
				float this_y = line->y_axis_selected - yoff;
				float this_xoff = yoff * 0.25F;

				if ((this_y + this_box.Height > last_y) && (last_xoff >= 0.0F)) {
					this_xoff = -this_box.Width - this_xoff;
				}

				ds->DrawTextLayout(desc, x_axis_selected + this_xoff, this_y,
					((line->close_color != nullptr) ? style.selected_color : line->color));

				last_xoff = this_xoff;
				last_y = this_box.Y + this_y;
			}
		}

		{ // draw selected time
			double selected_s = double(this->selected_x) / double(this->width) * double(ts->span) + double(ts->start);
			long long utc_s = (long long)std::round(selected_s);
			CanvasTextLayout^ timestamp = make_text_layout(make_daytimestamp_utc(utc_s, true), style.font);
			float xoff = timestamp->LayoutBounds.Width * 0.5F;

			ds->DrawTextLayout(timestamp, x_axis_selected - xoff, y + border_off, style.selected_color);
		}
	}
	
	ds->DrawRectangle(x + border_off, y + border_off,
		this->width - style.border_thickness, this->height - style.border_thickness,
		style.border_color, style.border_thickness);
}

void ITimeSerieslet::close_line(unsigned int idx, double alpha) {
	if (alpha == 0.0) {
		this->lines[idx].close_color = nullptr;
	} else {
		this->lines[idx].close_color = Colours::make(this->lines[idx].color, alpha);
	}
}

void ITimeSerieslet::hide_line(unsigned int idx, bool yes_no) {
	this->lines[idx].hiden = yes_no;
}

void ITimeSerieslet::set_value(unsigned int idx, double v) {
	long long now = current_milliseconds();
	TimeSeriesStyle style = this->get_style();
	bool datasource_loading = ((this->data_source != nullptr) && this->data_source->ready() && this->data_source->loading());
	
	if (datasource_loading) {
		this->begin_maniplation_sequence();
	}
	
	this->lines[idx].push_back_value(now, v);
	this->lines[idx].update_legend(this->precision + 1U, style);
	
	if (datasource_loading) {
		this->end_maniplation_sequence();
	}

	this->notify_updated();
}

void ITimeSerieslet::set_values(double* values, bool persistent) {
	long long now = current_milliseconds();
	TimeSeriesStyle style = this->get_style();
	bool datasource_ready = ((this->data_source != nullptr) && this->data_source->ready());
	
	this->begin_maniplation_sequence();
	
	for (unsigned int idx = 0; idx < this->count; idx++) {
		this->lines[idx].push_back_value(now, values[idx]);
		this->lines[idx].update_legend(this->precision + 1U, style);
	}

	if (persistent) {
		if (datasource_ready) {
			this->data_source->save(now, values, this->count);
		}
	}

	this->end_maniplation_sequence();
	
	this->notify_updated();
}

void ITimeSerieslet::begin_maniplation_sequence() {
	this->section.lock();
}

void ITimeSerieslet::on_datum_values(long long timepoint, double* values, unsigned int n) {
	for (unsigned int idx = 0; idx < this->count; idx++) {
		this->lines[idx].push_front_value(timepoint, values[idx]);
	}
}

void ITimeSerieslet::end_maniplation_sequence() {
	this->section.unlock();
}

void ITimeSerieslet::on_maniplation_complete(long long open_s, long long close_s) {
	this->next_loading_timepoint = close_s;
}

void ITimeSerieslet::own_caret(bool yes) {
	this->set_state(yes ? TimeSeriesState::History : TimeSeriesState::Realtime);
	this->update_horizontal_axes(this->get_style());
}

void ITimeSerieslet::on_tap(float x, float y) {
	this->selected_x = x;
}

bool ITimeSerieslet::on_key(VirtualKey key, bool screen_keyboard) {
	bool handled = false;

	switch (key) {
	case VirtualKey::PageUp: {
		this->history.start = current_seconds() - this->history_max;
		handled = true;
	}; break;
	case VirtualKey::Left: {
		this->history.start -= (this->history.span >> 3);
		this->history.start = std::max(this->history.start, current_seconds() - this->history_max);
		handled = true;
	}; break;
	case VirtualKey::Right: {
		this->history.start += (this->history.span >> 3);
		this->history.start = std::min(this->history.start, current_seconds());
		handled = true;
	}; break;
	case VirtualKey::PageDown: {
		this->history.start = current_seconds();
		handled = true;
	}; break;
	case VirtualKey::Add: {
		this->history.span = this->history.span >> 1;
		this->history.span = std::max(this->history.span, minute_span_s);
		handled = true;
	}; break;
	case VirtualKey::Subtract: {
		this->history.span = this->history.span << 1;
		this->history.span = std::min(this->history.span, day_span_s);
		handled = true;
	}; break;
	case VirtualKey::Escape: {
		this->history = this->realtime;
		handled = true;
	}; break;
	}

	if (handled) {
		this->selected_x = std::nanf("reset");
		this->update_horizontal_axes(this->get_style());
	}

	return handled;
}
