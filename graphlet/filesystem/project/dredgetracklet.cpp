#include "graphlet/filesystem/project/dredgetracklet.hpp"

#include "datum/file.hpp"
#include "datum/fixnum.hpp"
#include "datum/time.hpp"

#include "geometry.hpp"
#include "brushes.hxx"
#include "shape.hpp"
#include "paint.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

namespace {
	private enum class DT { DredgeTrack, Period, AfterImagePeriod, Visibility, Filter, Style };
	private enum Track { AfterImage, History };

	private struct tsdouble3 {
		long long timepoint;
		double3 dot;
	};
}

static const long long DEFAULT_SLOT_SIZE = 4096LL;
static const unsigned int DEFAULT_COUNT_RATE = 5;

static inline long long period_s(double period) {
	return (long long)flround(period * double(hour_span_s));
}

/*************************************************************************************************/
class WarGrey::DTPM::DredgeTracklet::Line {
public:
	~Line() noexcept {
		this->clear_pool();
	}

	Line(long long history_s, long long count_rate = DEFAULT_COUNT_RATE, long long slot_size = DEFAULT_SLOT_SIZE) {
		if (history_s <= 0) {
			history_s = 1LL;
		}

		if (slot_size > history_s * count_rate) {
			slot_size = history_s;
		}

		this->reset_pool(history_s, count_rate, slot_size);
	}

public:
	bool empty() {
		return ((this->dots == nullptr) || (this->dots[this->slot_count - 1] == nullptr));
	}

public:
	/** WARNING:
	 * Move cursor only when `this->empty()` returns `false`,
	 * so that there is no need to check the existence of current iterator slot every time.
	 */

	void cursor_end() {
		// NOTE: For reverse iteration, the current slot should greater than or equal to the history slot.
		this->virtual_iterator_slot = this->virtual_current_slot + this->slot_count;

		if (this->current_index > 0) {
			this->iterator_index = this->current_index - 1;
		} else {
			this->virtual_iterator_slot -= 1;
			this->iterator_index = this->slot_size - 1;
		}
	}

	bool cursor_step_backward(tsdouble3* dot) {
		bool has_value = false;

		if ((this->virtual_iterator_slot > this->virtual_history_slot)
			|| ((this->virtual_iterator_slot == this->virtual_history_slot)
				&& (this->iterator_index >= this->history_last_index))) {
			long long iterator_slot = this->virtual_iterator_slot % this->slot_count;

			(*dot) = this->dots[iterator_slot][this->iterator_index];

			if (this->iterator_index > 0) {
				this->iterator_index -= 1;
			} else {
				this->virtual_iterator_slot -= 1;
				this->iterator_index = this->slot_size - 1;
			}

			has_value = true;
		}

		return has_value;
	}

	void push_front_value(long long timestamp, double3& value) {
		long long current_slot = this->virtual_history_slot % this->slot_count;

		if (this->history_last_index > 0) {
			if (this->history_last_index == this->slot_size) {
				if (this->dots[current_slot] == nullptr) {
					this->bzero_slot(current_slot);
				}
			}

			this->dots[current_slot][this->history_last_index - 1].timepoint = timestamp;
			this->dots[current_slot][this->history_last_index - 1].dot = value;

			this->history_last_index -= 1;

			if ((this->history_last_index == 0) && (this->virtual_history_slot > this->virtual_current_slot + 1U)) {
				this->history_last_index = this->slot_size;
				this->virtual_history_slot -= 1;
			}
		}
	}

	void push_back_value(long long timestamp, double3& value) {
		long long current_slot = this->virtual_current_slot % this->slot_count;

		if (this->current_index == 0) {
			if (this->dots[current_slot] == nullptr) {
				this->bzero_slot(current_slot);
			}
		}

		this->dots[current_slot][this->current_index].timepoint = timestamp;
		this->dots[current_slot][this->current_index].dot = value;

		if (this->current_index < (this->slot_size - 1)) {
			this->current_index += 1;
		} else {
			if ((this->virtual_history_slot - this->virtual_current_slot) <= 1) {
				this->history_last_index = 0;
				this->virtual_history_slot += 1;
			}

			this->current_index = 0;
			this->virtual_current_slot += 1;
		}
	}

public:
	void reset_pool(long long history_s, long long count_rate, long long slot_size) {
		long long total = history_s * count_rate;
		long long slot_count = total / slot_size;

		this->clear_pool();

		/** NOTE
		 * The history values will not be stored in the current slot,
		 * thereby always adding an extra slot for the "air" history values.
		 *
		 * By the way, the pool is much bigger than desired.
		 */
		this->slot_count = (unsigned int)(slot_count + 1);
		this->slot_size = (unsigned int)(slot_size);

		this->dots = new tsdouble3*[this->slot_count];

		this->virtual_current_slot = 0;
		this->current_index = 0;

		this->virtual_history_slot = this->slot_count - 1;
		this->history_last_index = this->slot_size;

		for (unsigned int idx = 0; idx < this->slot_count; idx++) {
			this->dots[idx] = nullptr;
		}
	}

	void clear_pool() {
		if (this->dots != nullptr) {
			for (unsigned int idx = 0; idx < this->slot_count; idx++) {
				if (this->dots[idx] != nullptr) {
					delete[] this->dots[idx];
				}
			}

			delete[] this->dots;
			this->dots = nullptr;
		}
	}

	void bzero_slot(long long slot) {
		// `bzero()` is not necessary;
		this->dots[slot] = new tsdouble3[this->slot_size];
	}

private:
	tsdouble3** dots = nullptr;
	unsigned int slot_count = 0;
	unsigned int slot_size = 0;
	long long virtual_current_slot;
	long long current_index;
	long long virtual_history_slot;
	long long history_last_index;

private:
	long long virtual_iterator_slot;
	long long iterator_index;
};

/*************************************************************************************************/
DredgeTracklet::DredgeTracklet(ITrackDataSource* datasrc, Platform::String^ track, float width, float height, Platform::String^ ext, Platform::String^ rootdir)
	: width(width), height(height), track(nullptr), data_source(datasrc), preview_config(nullptr), after_image_outdated(true) {
	if (track != nullptr) {
		this->ms_appdata_config = ms_appdata_file(track, ext, rootdir);
	} else {
		// TODO: meanwhile it's useless and easy to be used incorrectly
		this->ms_appdata_config = ref new Uri(ms_apptemp_file("track", ext));
	}

	if (this->height == 0.0F) {
		this->height = this->width;
	} else if (this->height < 0.0F) {
		this->height *= -this->width;
	}

	if (this->data_source != nullptr) {
		this->data_source->reference();
	}

	this->last_dot.x = flnan;
	this->after_image_end = current_seconds();
}

DredgeTracklet::~DredgeTracklet() {
	this->unload(this->ms_appdata_config);

	if (this->data_source != nullptr) {
		this->data_source->destroy();
	}

	this->clear_lines(this->after_image_lines);
	this->clear_lines(this->realtime_lines);
	this->clear_lines(this->history_lines);
}

void DredgeTracklet::construct() {
	this->load(this->ms_appdata_config);
}

void DredgeTracklet::construct_line_if_necessary(unsigned int idx) {
	long long slot_size = DEFAULT_SLOT_SIZE;
	long long count_rate = DEFAULT_COUNT_RATE;

	if (this->after_image_lines[idx] == nullptr) {
		this->after_image_lines[idx] = new DredgeTracklet::Line(this->after_image_span, count_rate, slot_size);
	}

	if (this->realtime_lines[idx] == nullptr) {
		this->realtime_lines[idx] = new DredgeTracklet::Line(this->after_image_span, count_rate, slot_size);
	}

	if (this->history_lines[idx] == nullptr) {
		this->history_lines[idx] = new DredgeTracklet::Line(this->preview_config->end_timepoint - this->preview_config->begin_timepoint, count_rate, slot_size);
	}
}

void DredgeTracklet::update(long long count, long long interval, long long uptime) {
	/* NOTE: Ensure that loading from data source after loading track.config */

	if (this->ready()) {
		// loading after image tracks
		long long request_interval = this->after_image_span / 8;
		long long request_earliest_s = this->after_image_end - this->after_image_span;

		this->after_image_outdated = false;

		if (this->loading_after_image_timepoint > request_earliest_s) {
			long long close_s = this->loading_after_image_timepoint - request_interval;

			if (this->data_source != nullptr) {
				if (this->data_source->ready() && (!this->data_source->loading())) {
					this->data_source->load(this, Track::AfterImage, this->loading_after_image_timepoint, close_s);
				}
			} else {
				this->on_maniplation_complete(Track::AfterImage, this->loading_after_image_timepoint, close_s);
			}
		}

		if (this->loading_history_timepoint > this->preview_config->begin_timepoint) {
			long long close_s = this->loading_history_timepoint - request_interval;
			
			if (this->data_source != nullptr) {
				if (this->data_source->ready() && (!this->data_source->loading())) {
					this->data_source->load(this, Track::History, this->loading_history_timepoint, close_s);
				}
			} else {
				this->on_maniplation_complete(Track::History, this->loading_history_timepoint, close_s);
			}
		}
	}
}

void DredgeTracklet::on_appdata(Uri^ profile, DredgeTrack^ profile_config) {
	this->track_config = profile_config;

	// avoid updating raw instance accidently
	this->preview_config = ref new DredgeTrack(this->track_config);

	this->after_image_span = period_s(this->track_config->after_image_period);
	this->interval_squared = flsqr(this->preview_config->subinterval);

	// reload current after image lines in case the period is changed. 
	this->clear_lines(this->after_image_lines);
	this->loading_after_image_timepoint = this->after_image_end;
	this->after_image_outdated = true;

	// reload history lines in case the history range is changed.
	this->clear_lines(this->history_lines);
	this->loading_history_timepoint = this->preview_config->end_timepoint;
}

bool DredgeTracklet::ready() {
	return (this->preview_config != nullptr);
}

void DredgeTracklet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOX(w, this->width);
	SET_BOX(h, this->height);
}

void DredgeTracklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->master != nullptr) {
		if (this->preview_config != nullptr) {
			double partition_squared = flsqr(this->preview_config->partition_distance);
			long long now = current_milliseconds();
			long long ai_end = this->after_image_end * 1000LL;
			long long ai_span = this->after_image_span * 1000LL;

			for (unsigned idx = 0; idx < _N(DredgeTrackType); idx++) {
				if (this->preview_config->visibles[idx]) {
					this->draw_line(this->after_image_lines[idx], ds, x, y, ai_end - ai_span, ai_end, partition_squared);
					this->draw_line(this->realtime_lines[idx], ds, x, y, now - ai_span, now, partition_squared);

					this->draw_line(this->history_lines[idx], ds, x, y,
						this->preview_config->begin_timepoint * 1000L, this->preview_config->end_timepoint * 1000L,
						partition_squared);
				}
			}
		}
	}
}

void DredgeTracklet::draw_line(DredgeTracklet::Line* line, CanvasDrawingSession^ ds, float x, float y, long long start, long long end, double partition_squared) {
	double3 last_dot(flnan, 0.0, 0.0);
	tsdouble3 cursor;

	if (line != nullptr) {
		float minimum_diff = this->preview_config->track_width * 0.5F;
		float tolerance = this->preview_config->track_width;
		float2 last_pos;

		line->cursor_end();

		while (line->cursor_step_backward(&cursor)) {
			if ((cursor.timepoint >= start) && (cursor.timepoint <= end)) {
				if (!flisnan(last_dot.x)) {
					double distance_squared = points_distance_squared(last_dot.x, last_dot.y, cursor.dot.x, cursor.dot.y);
					float2 self_pos = this->master->position_to_local(cursor.dot.x, cursor.dot.y, x, y);

					if ((flabs(last_pos.x - self_pos.x) > tolerance) || (flabs(self_pos.y - last_pos.y) > tolerance)) {
						if (distance_squared <= partition_squared) {
							ds->DrawLine(last_pos, self_pos, Colours::Chocolate, this->preview_config->track_width);
						}

						last_pos = self_pos;
						last_dot = cursor.dot;
					}
				} else {
					last_pos = this->master->position_to_local(cursor.dot.x, cursor.dot.y, x, y);
					last_dot = cursor.dot;
				}
			}
		}
	}
}

void DredgeTracklet::attach_to_map(DigMaplet* master, bool force) {
	if (master != nullptr) {
		if (force || (this->master != master)) {
			this->notify_updated();
		}
	}

	this->master = master;
}

void DredgeTracklet::filter_dredging_dot(DredgeTrackType type, double3& dot, bool persistent, long long timepoint_ms) {
	if (this->ready()) {
		long long timepoint = ((timepoint_ms <= 0) ? current_milliseconds() : timepoint_ms);

		if (dot.z >= this->preview_config->depth0) {
			if (this->is_key_dot(dot)) {
				unsigned int type_id = _I(type);

				this->after_image_outdated = true;

				this->construct_line_if_necessary(type_id);
				this->realtime_lines[type_id]->push_back_value(timepoint, dot);

				if (persistent) {
					if ((this->data_source != nullptr) && this->data_source->ready()) {
						this->data_source->save(timepoint, type_id, dot);
					}
				}

				this->notify_updated();
			}
		} else {
			if (this->after_image_outdated) {
				if ((this->data_source != nullptr) && this->data_source->ready()) {
					if (!this->data_source->loading()) {
						this->clear_lines(this->realtime_lines);
						this->clear_lines(this->after_image_lines);

						this->after_image_end = timepoint / 1000LL;
						this->loading_after_image_timepoint = this->after_image_end;

						this->notify_updated();
					}
				}
			}
		}
	}
}

void DredgeTracklet::on_datum_values(uint8 id, long long open_s, long long timepoint_ms, long long type, double3& dot) {
	/* NOTE: Ensure that loading from data source after loading configuration */

	switch (id) {
	case Track::AfterImage: {
		if (this->loading_after_image_timepoint == open_s) {
			if (type < _N(DredgeTrackType)) {
				this->construct_line_if_necessary((unsigned int)type);
				this->after_image_lines[type]->push_front_value(timepoint_ms, dot);
			}
		}
	}; break;
	case Track::History: {
		if (this->loading_history_timepoint == open_s) {
			if (type < _N(DredgeTrackType)) {
				this->construct_line_if_necessary((unsigned int)type);
				this->history_lines[type]->push_front_value(timepoint_ms, dot);
			}
		}
	}; break;
	}
}

void DredgeTracklet::on_maniplation_complete(uint8 id, long long open_s, long long close_s) {
	switch (id) {
	case Track::AfterImage: {
		if (this->loading_after_image_timepoint == open_s) {
			this->loading_after_image_timepoint = close_s;
		}
	}; break;
	case Track::History: {
		if (this->loading_history_timepoint == open_s) {
			this->loading_history_timepoint = close_s;
		}
	}; break;
	}
}

bool DredgeTracklet::is_key_dot(double3& dot) {
	return flisnan(this->last_dot.x)
		|| (points_distance_squared(last_dot.x, last_dot.y, dot.x, dot.y)
			>= this->interval_squared);
}

void DredgeTracklet::clear_lines(DredgeTracklet::Line** lines) {
	for (unsigned int idx = 0; idx < _N(DredgeTrackType); idx++) {
		if (lines[idx] != nullptr) {
			delete lines[idx];
			lines[idx] = nullptr;
		}
	}

	if (this->realtime_lines == lines) {
		this->last_dot.x = flnan;
	}
}

/*************************************************************************************************/
DredgeTrack^ DredgeTracklet::clone_track(DredgeTrack^ dest, bool real_profile) {
	DredgeTrack^ clone = ((dest == nullptr) ? ref new DredgeTrack() : dest);

	clone->refresh(real_profile ? this->track_config : this->preview_config);

	return clone;
}

void DredgeTracklet::preview(DredgeTrack^ src) {
	if (src == nullptr) {
		this->preview_config->refresh(this->track_config);
	} else if (this->preview_config == nullptr) {
		this->preview_config = ref new DredgeTrack(src);
	} else {
		this->preview_config->refresh(src);
	}

	this->notify_updated();
}

void DredgeTracklet::refresh(DredgeTrack^ src) {
	this->store(this->ms_appdata_config, src);
}

/*************************************************************************************************/
DredgeTrack^ DredgeTrack::load(Platform::String^ path) {
	DredgeTrack^ dt = nullptr;
	size_t ptsize = sizeof(double2);
	Platform::String^ wtype;
	std::filebuf src;

	if (open_input_binary(src, path)) {
		dt = ref new DredgeTrack();
		wtype = read_wtext(src);
		discard_this_line(src);

		if (DT::DredgeTrack.ToString()->Equals(wtype)) {
			while (peek_char(src) != EOF) {
				wtype = read_wtext(src, char_end_of_word);

				if (DT::Period.ToString()->Equals(wtype)) {
					dt->begin_timepoint = read_integer(src);
					dt->begin_timepoint = read_integer(src);
				} else if (DT::AfterImagePeriod.ToString()->Equals(wtype)) {
					dt->after_image_period = read_flonum(src);
				} else if (DT::Filter.ToString()->Equals(wtype)) {
					dt->depth0 = read_flonum(src);
					dt->subinterval = read_flonum(src);
					dt->partition_distance = read_flonum(src);
				} else if (DT::Style.ToString()->Equals(wtype)) {
					dt->track_width = float(read_flonum(src));
				} else if (DT::Visibility.ToString()->Equals(wtype)) {
					long long n = read_natural(src);

					for (unsigned idx = 0; idx < _N(DredgeTrackType); idx++) {
						if (idx < n) {
							dt->visibles[idx] = read_bool(src);
						} else {
							dt->visibles[idx] = false;
						}
					}
				}

				discard_this_line(src);
			}
		}
	}

	return dt;
}

bool DredgeTrack::save(DredgeTrack^ self, Platform::String^ path) {
	std::wofstream v_config;
	bool okay = false;

	if (open_output_binary(v_config, path)) {
		write_wtext(v_config, DT::DredgeTrack, true);

		write_wtext(v_config, DT::Period);
		v_config << " " << self->begin_timepoint << " " << self->end_timepoint;
		write_newline(v_config);

		write_wtext(v_config, DT::AfterImagePeriod);
		v_config << " " << self->after_image_period;
		write_newline(v_config);

		write_wtext(v_config, DT::Visibility);
		v_config << " " << _N(DredgeTrackType);

		for (unsigned int idx = 0; idx < _N(DredgeTrackType); idx++) {
			v_config << " ";
			write_bool(v_config, self->visibles[idx]);
		}

		write_newline(v_config);

		write_wtext(v_config, DT::Filter);
		v_config << " " << self->depth0 << " " << self->subinterval << " " << self->partition_distance;
		write_newline(v_config);

		write_wtext(v_config, DT::Style);
		v_config << " " << self->track_width;
		write_newline(v_config);

		v_config.flush();

		okay = true;
	}

	return okay;
}

DredgeTrack::DredgeTrack(DredgeTrack^ src) {
	this->refresh(src);
}

void DredgeTrack::refresh(DredgeTrack^ src) {
	if ((src != nullptr) && (this != src)) {		
		this->depth0 = src->depth0;
		this->subinterval = src->subinterval;
		this->partition_distance = src->partition_distance;
		this->after_image_period = src->after_image_period;

		this->begin_timepoint = src->begin_timepoint;
		this->end_timepoint = src->end_timepoint;

		for (unsigned int idx = 0; idx < _N(DredgeTrackType); idx++) {
			this->visibles[idx] = src->visibles[idx];
		}

		this->track_width = src->track_width;
	}
}
