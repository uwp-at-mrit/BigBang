#include <list>

#include "graphlet/time/timelinelet.hpp"

#include "string.hpp"
#include "time.hpp"

#include "text.hpp"
#include "shape.hpp"
#include "geometry.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static const long long DEFAULT_SLOT_SIZE = 4096LL;

static CanvasTextFormat^ lines_default_font = make_bold_text_format(12.0F);
static CanvasSolidColorBrush^ default_timeline_color = Colours::RoyalBlue;
static CanvasSolidColorBrush^ default_time0_color = Colours::Green;
static CanvasSolidColorBrush^ default_footprint_color = Colours::Green;
static CanvasSolidColorBrush^ default_timen_color = Colours::Red;

/*************************************************************************************************/
Timelinelet::Timelinelet(long long tmin, long long tmax, float width, float thickness)
	: IRangelet(tmin, tmax), IStatelet(TimelineState::Stop), width(width), height(thickness) {
	this->enable_events(true);
}

void Timelinelet::construct() {
}

void Timelinelet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Timelinelet::prepare_style(TimelineState status, TimelineStyle& style) {
	CAS_SLOT(style.font, lines_default_font);
	CAS_SLOT(style.line_color, default_timeline_color);
	CAS_SLOT(style.time0_color, default_time0_color);
	CAS_SLOT(style.footprint_color, default_footprint_color);
	CAS_SLOT(style.timen_color, default_timen_color);
	CAS_SLOT(style.direction_color, style.footprint_color);
}

void Timelinelet::apply_style(TimelineStyle& style) {
	this->update_timepoints(this->vmin, this->vmax, style);
}

bool Timelinelet::can_change_range() {
	return (this->get_state() == TimelineState::Stop);
}

void Timelinelet::on_value_changed(long long timepoint) {
	TimelineStyle style = this->get_style();
}

void Timelinelet::on_range_changed(long long time0, long long timen) {
	this->update_timepoints(time0, timen, this->get_style());
}

void Timelinelet::update_timepoints(long long time0, long long timen, TimelineStyle& style) {
	this->date0_layout = make_text_layout(make_datestamp_utc(time0, false), style.font);
}

void Timelinelet::on_tap(float x, float y) {
}

void Timelinelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillRectangle(x, y, Width, Height, Colours::RoyalBlue);
}
