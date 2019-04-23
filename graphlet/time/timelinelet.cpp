#include <list>

#include "graphlet/time/timelinelet.hpp"

#include "datum/string.hpp"
#include "datum/flonum.hpp"
#include "datum/fixnum.hpp"
#include "datum/time.hpp"

#include "text.hpp"
#include "shape.hpp"
#include "polar.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasTextFormat^ lines_default_font = make_bold_text_format(10.0F);

static unsigned int default_speeds[] = { 1U, 2U, 3U, 4U, 5U };

private ref class TimelineStepper sealed : public ITimerListener {
internal:
	TimelineStepper(Timelinelet* master) : master(master) {}

internal:
	Syslog* get_logger() override {
		return this->master->get_logger();
	}

public:
	void on_elapse(long long count, long long interval, long long uptime) override {
		this->master->step();
	}

private:
	Timelinelet* master;
};

static inline void notify_timeline_state_changed(Timelinelet* self, ITimelineListener* observer) {
	switch (self->get_state()) {
	case TimelineState::Travel: observer->on_travel(self); break;
	case TimelineState::Service: observer->on_service(self); break;
	case TimelineState::Terminated: observer->on_terminate(self); break;
	}
}

/*************************************************************************************************/
Timelinelet::Timelinelet(long long tmin, long long tmax, float width, int frame_rate, float thickness)
	: Timelinelet(tmin, tmax, width, default_speeds, frame_rate, thickness) {}

Timelinelet::Timelinelet(long long tmin, long long tmax, float width, unsigned int* speeds, size_t speeds_count, int frame_rate, float thickness)
	: IRangelet(tmin, tmax), IStatelet(TimelineState::Terminated), width(width), thickness(thickness)
	, speeds_count(speeds_count), speed_shift(0), frame_rate(frame_rate) {
	TimelineStepper^ stepper = ref new TimelineStepper(this);

	this->enable_events(true);

	this->timer = ref new Timer(stepper, 0);
	this->speeds = new unsigned int[this->speeds_count];
	for (size_t idx = 0; idx < this->speeds_count; idx++) {
		this->speeds[idx] = fxmax(speeds[idx], 1U);
	}
}

Timelinelet::~Timelinelet() {
	delete[] this->speeds;
}

void Timelinelet::construct() {
	float radiusn = this->thickness * 1.618F;

	this->footprint_thickness = this->thickness * 1.2F;
	this->endpoint_radius = radiusn * 1.618F;
	this->icon_radius = this->endpoint_radius * 2.0F;

	this->cursor = polar_arrowhead(this->endpoint_radius * 1.2F, 0.0);
	this->endpoint0 = circle(this->endpoint_radius);
	this->endpointn = circle(radiusn);

	{ // make icons
		float half_pause_radiusX = this->icon_radius * 0.314F;
		float pause_diff = (this->icon_radius - half_pause_radiusX) * 0.5F;
		float half_speed_radius = this->icon_radius * 0.618F;
		float speed_diff = half_speed_radius * 1.2F * 0.5F;
		auto half_pause = polar_rectangle(half_pause_radiusX, this->icon_radius, 45.0F, 0.0);
		auto half_speed = polar_triangle(half_speed_radius, 0.0);
		
		this->travel_icon = polar_triangle(this->icon_radius, 0.0);
		this->pause_icon = geometry_union(half_pause, -pause_diff, 0.0F, half_pause, pause_diff);
		this->stop_icon = polar_rectangle(this->icon_radius, 45.0, 0.0);
		this->speed_icon = geometry_union(half_speed, -speed_diff, 0.0F, half_speed, speed_diff);
	}

	this->set_value(this->vmin, true);
}

void Timelinelet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

static CanvasSolidColorBrush^ default_timeline_color = Colours::DeepSkyBlue;
static CanvasSolidColorBrush^ default_footprint_color = Colours::LawnGreen;
static CanvasSolidColorBrush^ default_cursor_color = Colours::DodgerBlue;

void Timelinelet::prepare_style(TimelineState status, TimelineStyle& style) {
	CAS_SLOT(style.font, lines_default_font);
	CAS_SLOT(style.label_color, Colours::GhostWhite);

	CAS_SLOT(style.icon_border_color, Colours::GhostWhite);
	CAS_SLOT(style.icon_disabled_color, Colours::Transparent);
	CAS_SLOT(style.travel_icon_color, Colours::Green);
	CAS_SLOT(style.pause_icon_color, Colours::Orange);
	CAS_SLOT(style.stop_icon_color, Colours::Red);
	CAS_SLOT(style.speed_icon_color, Colours::Blue);
	
	switch (status) {
	case TimelineState::Terminated: CAS_VALUES(style.line_color, Colours::GrayText, style.cursor_color, Colours::Gray); break;
	case TimelineState::Service: CAS_SLOT(style.line_color, style.pause_icon_color); break;
	}

	CAS_SLOT(style.line_color, Colours::DeepSkyBlue);
	CAS_SLOT(style.cursor_color, Colours::SpringGreen);
	CAS_SLOT(style.footprint_color, Colours::LawnGreen);
}

void Timelinelet::apply_style(TimelineStyle& style) {
	this->update_time_range();
	this->update_speed_shift();
}

void Timelinelet::on_state_changed(TimelineState state) {
	switch (state) {
	case TimelineState::Travel: this->timer->start(this->frame_rate, this->speeds[this->speed_shift]); break;
	case TimelineState::Service: this->timer->stop(); break;
	case TimelineState::Terminated: {
		this->timer->stop();
		this->set_value(this->vmin);
		this->footprints.clear();
	}; break;
	}

	for (auto observer : this->obsevers) {
		notify_timeline_state_changed(this, observer);
	}
}

void Timelinelet::on_value_changed(long long timepoint_ms) {
	TimelineStyle style = this->get_style();
	long long utc = timepoint_ms / 1000LL;

	this->footprints.push_back(timepoint_ms);
	this->moment_date = make_text_layout(make_datestamp_utc(utc, true), style.font);
	this->moment_time = make_text_layout(make_daytimestamp_utc(utc, true), style.font);
}

bool Timelinelet::can_change_range() {
	return (this->get_state() == TimelineState::Terminated);
}

void Timelinelet::on_range_changed(long long time0, long long timen) {
	this->update_time_range();
	this->set_value(this->vmin);
	
	for (auto observer : this->obsevers) {
		observer->on_startover(this, this->vmin, this->vmax);
	}
}

void Timelinelet::on_tap(float x, float y) {
	TimelineState state = this->get_state();
	float travle_rx = this->icon_radius * 2.0F;
	float terminate_lx = travle_rx + this->endpoint_radius * 2.0F;
	float terminate_rx = terminate_lx + travle_rx;
	float speedx_lx = this->width - travle_rx;
	float tl_lx = this->timeline_lx - this->endpoint_radius;
	float tl_rx = this->timeline_rx + this->endpoint_radius;
	bool handled = true;

	if (x <= travle_rx) {
		this->set_state((state == TimelineState::Travel) ? TimelineState::Service : TimelineState::Travel);
	} else if ((x >= terminate_lx) && (x <= terminate_rx)) {
		this->set_state(TimelineState::Terminated);
	} else if (x >= speedx_lx) {
		this->shift_speed();
	} else if ((x >= tl_lx) && (x <= tl_rx) && (state != TimelineState::Terminated)) {
		float percentage = flmax(flmin((x - this->timeline_lx) / (this->timeline_rx - this->timeline_lx), 1.0F), 0.0F);
		long long this_timepoint = fxround(this->vmax - this->vmin, percentage) + this->vmin;

		for (auto observer : this->obsevers) {
			observer->on_time_skipped(this, this_timepoint);
		}
	} else {
		handled = false;
	}

	if (handled) {
		this->notify_updated();
	}
}

void Timelinelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	TimelineStyle style = this->get_style();
	TimelineState state = this->get_state();
	float cy = y + this->height * 0.5F;
	float lx = x + this->timeline_lx;
	float rx = x + this->timeline_rx;
	float length = (rx - lx);
	float cursor_x = lx + length * float(this->get_percentage());
	
	ds->DrawCachedGeometry(this->timepoints, x, y, style.label_color);
	ds->DrawLine(lx + this->endpoint_radius, cy, rx - this->endpoint_radius, cy, style.line_color, this->thickness);

	{ // draw footprints
		float fpdiff = this->footprint_thickness;
		float dot_r = this->footprint_thickness * 0.618F;
		float fpx0 = lx;
		float last_fpx = lx;
		
		for (auto it = this->footprints.begin(); it != this->footprints.end(); it++) {
			float fpx = lx + length * float(this->get_percentage(*it));

			if ((fpx < last_fpx) || ((fpx - last_fpx) > fpdiff)) {
				if (fpx0 != last_fpx) {
					ds->DrawLine(fpx0, cy, last_fpx, cy, style.footprint_color, this->footprint_thickness);
				} else {
					ds->FillCircle(fpx0, cy, dot_r, style.footprint_color);
				}

				fpx0 = fpx;
			}

			last_fpx = fpx;
		}

		if (fpx0 != last_fpx) {
			ds->DrawLine(fpx0, cy, last_fpx, cy, style.footprint_color, this->footprint_thickness);
		} else {
			ds->FillCircle(fpx0, cy, dot_r, style.footprint_color);
		}
	}

	ds->FillGeometry(this->endpoint0, lx, cy, style.travel_icon_color);
	ds->DrawGeometry(this->endpoint0, lx, cy, style.line_color);
	ds->FillGeometry(this->endpointn, rx, cy, style.stop_icon_color);
	ds->DrawGeometry(this->endpoint0 /* it's not a typo */, rx, cy, style.line_color);

	{ // draw cursor
		Rect tlbox = this->moment_time->LayoutBounds;
		Rect dlbox = this->moment_date->LayoutBounds;
		Rect tdbox = this->moment_time->DrawBounds;
		Rect ddbox = this->moment_date->DrawBounds;

		ds->FillGeometry(this->cursor, cursor_x, cy, style.cursor_color);
		ds->DrawGeometry(this->cursor, cursor_x, cy, style.line_color);

		ds->DrawTextLayout(this->moment_time, cursor_x - tlbox.Width * 0.5F, y - tdbox.Y, style.label_color);
		ds->DrawTextLayout(this->moment_date, cursor_x - dlbox.Width * 0.5F, y + this->height - (ddbox.Y + ddbox.Height), style.label_color);
	}

	{ // draw icons
		float tx = x + this->icon_radius;
		float sx = tx + (this->endpoint_radius + this->icon_radius) * 2.0F;
		float xx = x + this->width - this->icon_radius - 1.0F;
		
		if (state == TimelineState::Travel) {
			ds->FillGeometry(this->pause_icon, tx, cy, style.pause_icon_color);
			ds->DrawGeometry(this->pause_icon, tx, cy, style.icon_border_color);
		} else {
			ds->FillGeometry(this->travel_icon, tx, cy, style.travel_icon_color);
			ds->DrawGeometry(this->travel_icon, tx, cy, style.icon_border_color);
		}

		if (state == TimelineState::Terminated) {
			ds->FillGeometry(this->stop_icon, sx, cy, style.icon_disabled_color);
		} else {
			ds->FillGeometry(this->stop_icon, sx, cy, style.stop_icon_color);
			ds->DrawGeometry(this->stop_icon, sx, cy, style.icon_border_color);
		}

		if (this->speeds_count > 1) {
			ds->FillGeometry(this->speed_icon, xx, cy, style.speed_icon_color);
			ds->DrawGeometry(this->speed_icon, xx, cy, style.icon_border_color);
			ds->DrawTextLayout(this->speedx, xx - this->speedx->LayoutBounds.Width * 0.5F, cy + this->icon_radius * 0.5F, style.label_color);
		} else {
			ds->FillGeometry(this->speed_icon, xx, cy, style.icon_disabled_color);
		}
	}
}

void Timelinelet::push_event_listener(ITimelineListener* observer) {
	if (observer != nullptr) {
		this->obsevers.push_back(observer);
		observer->on_startover(this, this->vmin, this->vmax);

		// TODO: is this necessary?
		notify_timeline_state_changed(this, observer);
	}
}

long long Timelinelet::get_departure_timepoint() {
	return this->vmin;
}

long long Timelinelet::get_destination_timepoint() {
	return this->vmax;
}

unsigned int Timelinelet::get_speed_shift() {
	return this->speeds[this->speed_shift];
}

void Timelinelet::shift_speed() {
	if (this->speeds_count > 1) {
		this->speed_shift = (this->speed_shift + 1) % this->speeds_count;
		this->update_speed_shift();

		for (auto observer : this->obsevers) {
			observer->on_speed_shifted(this, this->speeds[this->speed_shift]);
		}

		if (this->get_state() == TimelineState::Travel) {
			this->timer->start(this->frame_rate, this->speeds[this->speed_shift]);
		}
	}
}

void Timelinelet::step() {
	if (this->get_state() == TimelineState::Travel) {
		for (auto observer : this->obsevers) {
			observer->on_step(this);
		}
	}
}

/*************************************************************************************************/
void Timelinelet::update_speed_shift() {
	TimelineStyle style = this->get_style();
	unsigned int x = this->speeds[this->speed_shift];

	this->speedx = make_text_layout(x.ToString() + "x", style.font);
}

void Timelinelet::update_time_range() {
	TextExtent dbox0, tbox0, dboxn, tboxn;
	TimelineStyle style = this->get_style();
	long long departure = this->vmin / 1000LL;
	long long destination = this->vmax / 1000LL;
	auto dp0 = paragraph(make_datestamp_utc(departure, true), style.font, &dbox0);
	auto tp0 = paragraph(make_daytimestamp_utc(departure, true), style.font, &tbox0);
	auto dpn = paragraph(make_datestamp_utc(destination, true), style.font, &dboxn);
	auto tpn = paragraph(make_daytimestamp_utc(destination, true), style.font, &tboxn);
	float dwhalf0 = dbox0.width * 0.5F;
	float twhalf0 = tbox0.width * 0.5F;
	float dwhalfn = dboxn.width * 0.5F;
	float twhalfn = tboxn.width * 0.5F;
	float gridsize = (this->icon_radius + this->endpoint_radius) * 2.0F;
	float rx = this->width - gridsize * 1.0F;
	float lx = gridsize * 2.0F;

	this->height = fmaxf(dbox0.height + tbox0.height, dboxn.height + tboxn.height) + this->endpoint_radius * 1.618F;
	this->timeline_lx = lx + fmaxf(dwhalf0, twhalf0);
	this->timeline_rx = fmaxf(rx - fmaxf(dwhalfn, twhalfn), this->timeline_lx + 1.0F);

	{ // merge timepoints
		auto dt0 = geometry_union(dp0, this->timeline_lx - dwhalf0, this->height - dbox0.height + dbox0.bspace,
			tp0, this->timeline_lx - twhalf0, -tbox0.tspace);

		auto dtn = geometry_union(dpn, this->timeline_rx - dwhalfn, this->height - dboxn.height + dbox0.bspace,
			tpn, this->timeline_rx - twhalfn, -tboxn.tspace);

		this->timepoints = geometry_freeze(geometry_union(dt0, dtn));
	}
}
