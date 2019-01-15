#include <list>

#include "graphlet/time/timelinelet.hpp"

#include "string.hpp"
#include "time.hpp"

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

static unsigned int default_speeds[] = { 1U, 2U, 4U, 8U, 16U };

/*************************************************************************************************/
Timelinelet::Timelinelet(long long tmin, long long tmax, float width, float thickness)
	: Timelinelet(tmin, tmax, width, default_speeds, thickness) {}

Timelinelet::Timelinelet(long long tmin, long long tmax, float width, unsigned int* speeds, size_t speeds_count, float thickness)
	: IRangelet(tmin, tmax), IStatelet(TimelineState::Stop), width(width), thickness(thickness), speeds_count(speeds_count), speed_index(0) {
	this->enable_events(true);

	this->speeds = new unsigned int[this->speeds_count];
	for (size_t idx = 0; idx < this->speeds_count; idx++) {
		this->speeds[idx] = std::max(speeds[idx], 1U);
	}
}

Timelinelet::~Timelinelet() {
	delete[] this->speeds;
}

void Timelinelet::construct() {
	float radiusn = this->thickness * 1.618F;

	this->footprint_radius = this->thickness;
	this->endpoint_radius = radiusn * 1.618F;
	this->icon_radius = this->endpoint_radius * 2.0F;

	this->cursor = polar_arrowhead(this->endpoint_radius * 1.2F, 0.0);
	this->footprint = geometry_freeze(circle(this->footprint_radius));
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
	case TimelineState::Stop: CAS_VALUES(style.line_color, Colours::GrayText, style.cursor_color, Colours::Gray); break;
	case TimelineState::Pause: CAS_SLOT(style.line_color, style.pause_icon_color); break;
	}

	CAS_SLOT(style.line_color, Colours::DeepSkyBlue);
	CAS_SLOT(style.cursor_color, Colours::SpringGreen);
	CAS_SLOT(style.footprint_color, Colours::LawnGreen);
}

void Timelinelet::apply_style(TimelineStyle& style) {
	this->on_range_changed(this->vmin, this->vmax); // implies `set_value`
	this->on_speed_changed();
}

void Timelinelet::on_state_changed(TimelineState state) {
	Syslog* logger = this->get_logger();

	if (state == TimelineState::Stop) {
		this->set_value(this->vmin);
		this->footprints.clear();
	}

	for (auto observer : this->obsevers) {
		switch (state) {
		case TimelineState::Travel: observer->on_launch(this, logger); break;
		case TimelineState::Pause: observer->on_pause(this, logger); break;
		case TimelineState::Stop: observer->on_terminate(this, logger); break;
		}
	}
}

void Timelinelet::on_value_changed(long long timepoint) {
	TimelineStyle style = this->get_style();

	this->footprints.push_back(timepoint);
	this->step = make_text_layout(make_daytimestamp_utc(timepoint, true), style.font);
}

void Timelinelet::on_speed_changed() {
	TimelineStyle style = this->get_style();
	Syslog* logger = this->get_logger();
	unsigned int x = this->speeds[this->speed_index];
	
	this->speedx = make_text_layout(x.ToString() + "x", style.font);

	for (auto observer : this->obsevers) {
		observer->on_speed_changed(this, x, logger);
	}
}

bool Timelinelet::can_change_range() {
	return (this->get_state() == TimelineState::Stop);
}

void Timelinelet::on_range_changed(long long time0, long long timen) {
	TextExtent dbox0, tbox0, dboxn, tboxn;
	TimelineStyle style = this->get_style();
	auto dp0 = paragraph(make_datestamp_utc(time0, true), style.font, &dbox0);
	auto tp0 = paragraph(make_daytimestamp_utc(time0, true), style.font, &tbox0);
	auto dpn = paragraph(make_datestamp_utc(timen, true), style.font, &dboxn);
	auto tpn = paragraph(make_daytimestamp_utc(timen, true), style.font, &tboxn);
	float dwhalf0 = dbox0.width * 0.5F;
	float twhalf0 = tbox0.width * 0.5F;
	float dwhalfn = dboxn.width * 0.5F;
	float twhalfn = tboxn.width * 0.5F;
	float gridsize = (this->icon_radius + this->endpoint_radius) * 2.0F;
	float rx = this->width - gridsize * 1.0F;
	float lx = gridsize * 2.0F;

	this->height = std::fmaxf(dbox0.height + tbox0.height, dboxn.height + tboxn.height) + this->endpoint_radius * 1.618F;
	this->timeline_lx = lx + std::fmaxf(dwhalf0, twhalf0);
	this->timeline_rx = std::fmaxf(rx - std::fmaxf(dwhalfn, twhalfn), this->timeline_lx + 1.0F);

	{ // merge timepoints
		auto dt0 = geometry_union(dp0, this->timeline_lx - dwhalf0, this->height - dbox0.height + dbox0.bspace,
			tp0, this->timeline_lx - twhalf0, -tbox0.tspace);

		auto dtn = geometry_union(dpn, this->timeline_rx - dwhalfn, this->height - dboxn.height + dbox0.bspace,
			tpn, this->timeline_rx - twhalfn, -tboxn.tspace);
		
		this->timepoints = geometry_freeze(geometry_union(dt0, dtn));
	}

	this->set_value(this->vmin);
	
	{ // broadcast event
		Syslog* logger = this->get_logger();

		for (auto observer : this->obsevers) {
			observer->on_startover(this, this->vmin, this->vmax, logger);
		}
	}
}

void Timelinelet::on_tap(float x, float y) {
	TimelineState state = this->get_state();
	float trx = this->icon_radius * 2.0F;
	float slx = trx + this->endpoint_radius * 2.0F;
	float srx = slx + trx;
	float xlx = this->width - trx;
	bool handled = true;

	if (x <= trx) {
		this->set_state((TimelineState::Travel == state) ? TimelineState::Pause : TimelineState::Travel);
	} else if ((x >= slx) && (x <= srx)) {
		this->set_state(TimelineState::Stop);
	} else if (x >= xlx) {
		this->speed_index = (this->speed_index + 1) % this->speeds_count;
		this->on_speed_changed();
	} else if ((x >= this->timeline_lx) && (x <= this->timeline_rx) && (state != TimelineState::Stop)) {
		float percentage = (x - this->timeline_lx) / (this->timeline_rx - this->timeline_lx);
		long long this_timepoint = ((long long)(std::roundf(float(this->vmax - this->vmin) * percentage))) + this->vmin;

		this->set_value(this_timepoint);
		
		{ // broadcast event
			Syslog* logger = this->get_logger();

			for (auto observer : this->obsevers) {
				observer->on_time_skipped(this, this_timepoint, logger);
			}
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
	float range = (rx - lx);
	float cursor_x = lx + range * float(this->get_percentage());
	
	ds->DrawCachedGeometry(this->timepoints, x, y, style.label_color);
	ds->DrawLine(lx + this->endpoint_radius, cy, rx - this->endpoint_radius, cy, style.line_color, this->thickness);

	for (auto it = this->footprints.begin(); it != this->footprints.end(); it++) {
		ds->DrawCachedGeometry(this->footprint, lx + range * float(this->get_percentage(*it)), cy, style.footprint_color);
	}

	ds->FillGeometry(this->endpoint0, lx, cy, style.travel_icon_color);
	ds->DrawGeometry(this->endpoint0, lx, cy, style.line_color);
	ds->FillGeometry(this->endpointn, rx, cy, style.stop_icon_color);
	ds->DrawGeometry(this->endpoint0 /* it's not a typo */, rx, cy, style.line_color);

	ds->FillGeometry(this->cursor, cursor_x, cy, style.cursor_color);
	ds->DrawGeometry(this->cursor, cursor_x, cy, style.line_color);

	ds->DrawTextLayout(this->step, cursor_x - this->step->LayoutBounds.Width * 0.5F, y - this->step->DrawBounds.Y, style.label_color);

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

		if (state == TimelineState::Stop) {
			ds->FillGeometry(this->stop_icon, sx, cy, style.icon_disabled_color);
		} else {
			ds->FillGeometry(this->stop_icon, sx, cy, style.stop_icon_color);
			ds->DrawGeometry(this->stop_icon, sx, cy, style.icon_border_color);
		}

		ds->FillGeometry(this->speed_icon, xx, cy, style.speed_icon_color);
		ds->DrawGeometry(this->speed_icon, xx, cy, style.icon_border_color);
		ds->DrawTextLayout(this->speedx, xx - this->speedx->LayoutBounds.Width * 0.5F, cy + this->icon_radius * 0.5F, style.label_color);
	}
}

void Timelinelet::push_event_listener(ITimelineListener* observer) {
	if (observer != nullptr) {
		this->obsevers.push_back(observer);
	}
}

long long Timelinelet::get_departure_timepoint() {
	return this->vmin;
}

long long Timelinelet::get_destination_timepoint() {
	return this->vmax;
}
