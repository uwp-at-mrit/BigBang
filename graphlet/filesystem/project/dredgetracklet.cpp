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
}

static const long long DEFAULT_SLOT_SIZE = 4096LL;
static const unsigned int DEFAULT_COUNT_RATE = 5;

/*************************************************************************************************/
class WarGrey::DTPM::DredgeTracklet::Line {
public:
	~Line() noexcept {
		this->clear_pool();
	}

	Line(long long history_s, long long count_rate = DEFAULT_COUNT_RATE, long long slot_size = DEFAULT_SLOT_SIZE) {
		if (slot_size > history_s * count_rate) {
			slot_size = history_s;
		}

		this->reset_pool(history_s, count_rate, slot_size);
	}

public:
	bool empty() {
		return (this->dots[this->slot_count - 1] == nullptr);
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

	bool cursor_step_backward(double3* dot) {
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

			this->dots[current_slot][this->history_last_index - 1] = value;

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

		this->dots[current_slot][this->current_index] = value;

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

		this->dots = new double3*[this->slot_count];

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
		}
	}

	void bzero_slot(long long slot) {
		// `bzero()` is not necessary;
		this->dots[slot] = new double3[this->slot_size];
	}

private:
	double3** dots = nullptr;
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
	: width(width), height(height), track(nullptr), data_source(datasrc) {
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
}

DredgeTracklet::~DredgeTracklet() {
	this->unload(this->ms_appdata_config);

	if (this->data_source != nullptr) {
		this->data_source->destroy();
	}

	for (unsigned int idx = 0; idx < _N(DredgeTrackType); idx++) {
		if (this->lines[idx] != nullptr) {
			delete this->lines[idx];
			this->lines[idx] = nullptr;
		}
	}
}

void DredgeTracklet::construct() {
	this->load(this->ms_appdata_config);
}

void DredgeTracklet::construct_line_if_necessary(unsigned int idx) {
	long long slot_size = DEFAULT_SLOT_SIZE;
	long long count_rate = DEFAULT_COUNT_RATE;

	if (this->lines[idx] == nullptr) {
		this->lines[idx] = new DredgeTracklet::Line(this->history_span, count_rate, slot_size);
	}
}

void DredgeTracklet::update(long long count, long long interval, long long uptime) {
	/* NOTE: Ensure that loading from data source after loading configuration */

	if (this->ready()) {
		long long limit = this->history_destination;

		if (this->history_destination <= 0) {
			limit = current_seconds();
		}

		{ // load exists data
			long long request_interval = this->history_span / 8;
			long long request_earliest_s = limit - this->history_span;

			if (this->loading_timepoint > request_earliest_s) {
				if (this->data_source != nullptr) {
					if (this->data_source->ready() && (!this->data_source->loading())) {
						this->data_source->load(this, this->loading_timepoint, (this->loading_timepoint - request_interval));
					}
				} else {
					this->on_maniplation_complete(this->loading_timepoint, (this->loading_timepoint - request_interval));
				}
			}
		}
	}
}

void DredgeTracklet::on_appdata(Uri^ profile, DredgeTrack^ profile_config) {
	this->track_config = profile_config;

	// avoid updating raw instance accidently
	this->preview_config = ref new DredgeTrack(this->track_config);

	this->history_span = (long long)flround(this->track_config->after_image_period * double(hour_span_s));
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
			for (unsigned idx = 0; idx < _N(DredgeTrackType); idx++) {
				if (this->preview_config->visibles[idx]) {
					DredgeTracklet::Line* line = this->lines[idx];
					double3 cursor_dot;

					if (line != nullptr) {
						float minimum_diff = this->preview_config->track_width * 0.5F;
						float tolerance = this->preview_config->track_width;
						float2 last_pos(nanf("no dot"), 0.0F);

						line->cursor_end();

						while (line->cursor_step_backward(&cursor_dot)) {
							float2 this_pos = this->master->position_to_local(cursor_dot.x, cursor_dot.y, x, y);

							if (!isnan(last_pos.x)) {
								if ((flabs(last_pos.x - this_pos.x) > tolerance) || (flabs(this_pos.y - last_pos.y) > tolerance)) {
									ds->DrawLine(last_pos, this_pos, Colours::Chocolate, this->preview_config->track_width);

									last_pos = this_pos;
								}
							} else {
								last_pos = this_pos;
							}
						}
					}
				}
			}
		}
	} else {
		ds->DrawRectangle(x, y, this->width, this->height, Colours::DodgerBlue, 1.0F);
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

void DredgeTracklet::push_track_dot(DredgeTrackType type, double3& dot, bool persistent, long long timepoint_ms) {
	if (this->ready()) {
		long long timepoint = ((timepoint_ms <= 0) ? current_milliseconds() : timepoint_ms);
		long long limit = ((this->history_destination <= 0) ? timepoint : (this->history_destination * 1000LL));

		if ((timepoint <= limit) && (timepoint >= (limit - this->history_span * 1000LL))) {
			unsigned int type_id = _I(type);

			this->construct_line_if_necessary(type_id);
			this->lines[type_id]->push_back_value(timepoint, dot);

			if (persistent) {
				if ((this->data_source != nullptr) && this->data_source->ready()) {
					this->data_source->save(timepoint, type_id, dot);
				}
			}

			this->notify_updated();
		}
	}
}

void DredgeTracklet::on_datum_values(long long open_s, long long timepoint_ms, long long type, double3& dot) {
	/* NOTE: Ensure that loading from data source after loading configuration */

	if (this->loading_timepoint == open_s) {
		if (type < _N(DredgeTrackType)) {
			this->construct_line_if_necessary((unsigned int)type);
			this->lines[type]->push_front_value(timepoint_ms, dot);
			this->get_logger()->log_message(Log::Info, L"loaded: %lf, %lf, %lf", dot.x, dot.y, dot.z);
		}
	}
}

void DredgeTracklet::on_maniplation_complete(long long open_s, long long close_s) {
	if (this->loading_timepoint == open_s) {
		this->loading_timepoint = close_s;
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
					dt->begin_timepoint = read_natural(src);
					dt->begin_timepoint = read_natural(src);
				} else if (DT::AfterImagePeriod.ToString()->Equals(wtype)) {
					dt->after_image_period = read_flonum(src);
				} else if (DT::Visibility.ToString()->Equals(wtype)) {
					long long n = read_natural(src);

					for (unsigned idx = 0; idx < _N(DredgeTrackType); idx++) {
						if (idx < n) {
							dt->visibles[idx] = read_bool(src);
						} else {
							dt->visibles[idx] = false;
						}
					}
				} else if (DT::Filter.ToString()->Equals(wtype)) {
					dt->depth0 = read_flonum(src);
					dt->interval = read_flonum(src);
				} else if (DT::Style.ToString()->Equals(wtype)) {
					dt->track_width = float(read_flonum(src));
					dt->track_color = (unsigned int)read_natural(src);
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
		v_config << " " << self->depth0 << " " << self->interval;
		write_newline(v_config);

		write_wtext(v_config, DT::Style);
		v_config << " " << self->track_width << " " << self->track_color;
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
		this->interval = src->interval;
		this->after_image_period = src->after_image_period;

		this->begin_timepoint = src->begin_timepoint;
		this->end_timepoint = src->end_timepoint;

		for (unsigned int idx = 0; idx < _N(DredgeTrackType); idx++) {
			this->visibles[idx] = src->visibles[idx];
		}

		this->track_width = src->track_width;
		this->track_color = src->track_color;
	}
}
