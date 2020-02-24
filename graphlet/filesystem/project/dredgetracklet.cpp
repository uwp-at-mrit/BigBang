#include "graphlet/filesystem/project/dredgetracklet.hpp"

#include "datum/file.hpp"
#include "datum/fixnum.hpp"

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

/*************************************************************************************************/
DredgeTracklet::DredgeTracklet(ITrackDataSource* src, Platform::String^ track, float width, float height, Platform::String^ ext, Platform::String^ rootdir)
	: width(width), height(height), track(nullptr) {
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
}

DredgeTracklet::~DredgeTracklet() {
	this->unload(this->ms_appdata_config);
}

void DredgeTracklet::construct() {
	this->load(this->ms_appdata_config);
}

void DredgeTracklet::update(long long count, long long interval, long long uptime) {
	if (this->ready()) {
		//long long limit = this->history_destination;

		//if (this->history_destination <= 0) {
			//limit = current_seconds();
			//this->check_visual_window(limit);
		//}

		{ // load exists data
			//long long request_interval = this->history_span / this->realtime.step;
			//long long request_earliest_s = fxmin(this->realtime.start, limit - this->history_span);

			//if (this->loading_timepoint > request_earliest_s) {
				//if (this->data_source != nullptr) {
					//if (this->data_source->ready() && (!this->data_source->loading())) {
						//this->data_source->load(this, this->loading_timepoint, (this->loading_timepoint - request_interval));
					//}
				//} else {
					//this->on_maniplation_complete(this->loading_timepoint, (this->loading_timepoint - request_interval));
				//}
			//}
		}
	}
}

void DredgeTracklet::on_appdata(Uri^ profile, DredgeTrack^ profile_config) {
	this->track_config = profile_config;

	// avoid updating raw instance accidently
	this->preview_config = ref new DredgeTrack(this->track_config);
}

bool DredgeTracklet::ready() {
	return (this->preview_config != nullptr);
}

void DredgeTracklet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOX(w, this->width);
	SET_BOX(h, this->height);
}

void DredgeTracklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float border_off = 0.0F;// this->style.border_thickness * 0.5F;
	float half_width = this->width * 0.5F;
	float cx = x + half_width;

	if (this->preview_config != nullptr) {
		//ds->DrawCachedGeometry(this->haxes, x, y, this->style.haxes_color);
		//ds->DrawCachedGeometry(this->vaxes, x, y, this->style.vaxes_color);

		//ds->DrawCachedGeometry(this->hmarks, x, y, this->style.haxes_color);
		//ds->DrawCachedGeometry(this->vmarks, x, y, this->style.vaxes_color);
	}

	//ds->DrawLine(cx, y, cx, y + this->height, this->style.centerline_color, this->style.centerline_thickness, this->style.centerline_style);

	if (this->track != nullptr) {
		//ds->DrawGeometry(this->profile, cx, y, this->style.profile_color, this->style.profile_thickness, this->style.profile_style);
	}

	ds->DrawRectangle(x + border_off, y + border_off, this->width, this->height,
		Colours::DodgerBlue, 1.0F);
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
					dt->ps_visible = read_bool(src);
					dt->sb_visible = read_bool(src);
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
		v_config << " ";
		write_bool(v_config, self->ps_visible);
		v_config << " ";
		write_bool(v_config, self->sb_visible);
		write_newline(v_config);

		write_wtext(v_config, DT::Filter);
		v_config << " " << self->depth0 << " " << self->interval;
		write_newline(v_config);

		write_wtext(v_config, DT::Style);
		v_config << " " << self->track_color << " " << self->track_color;
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
		this->ps_visible = src->ps_visible;
		this->sb_visible = src->sb_visible;

		this->track_width = src->track_width;
		this->track_color = src->track_color;
	}
}

void DredgeTracklet::on_datum_values(long long open_s, long long timepoint_ms, long long group, double3& dot) {
	//if (this->loading_timepoint == open_s) {
		//for (unsigned int idx = 0; idx < this->count; idx++) {
			//this->lines[idx].push_front_value(timepoint_ms, values[idx]);
		//}
	//}
}

void DredgeTracklet::on_maniplation_complete(long long open_s, long long close_s) {
	//if (this->loading_timepoint == open_s) {
		//this->loading_timepoint = close_s;
	//}
}
