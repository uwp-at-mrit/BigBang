#include "graphlet/filesystem/project/dragtracklet.hpp"

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
	private enum class DT { DragheadTrack, Period, AfterImagePeriod, Visibility, Filter, Style };
}

/*************************************************************************************************/
DragTracklet::DragTracklet(IVessellet* vessel, Platform::String^ profile, float width, float height, Platform::String^ ext, Platform::String^ rootdir)
	: width(width), height(height), direction_sign(1.0), profile(nullptr), vessel(vessel) {
	if (profile != nullptr) {
		this->ms_appdata_config = ms_appdata_file(profile, ext, rootdir);
	} else {
		// TODO: meanwhile it's useless and easy to be used incorrectly
		this->ms_appdata_config = ref new Uri(ms_apptemp_file("profile", ext));
	}

	if (this->height == 0.0F) {
		this->height = 200.0F;
	} else if (this->height < 0.0F) {
		this->height *= -this->width;
	}
}

DragTracklet::~DragTracklet() {
	this->unload(this->ms_appdata_config);

	if (this->outline) {
		delete this->outline;
	}
}

void DragTracklet::construct() {
	this->load(this->ms_appdata_config);
}

void DragTracklet::update_outline(const Outline* outline, double vessel_x, double vessel_y) {
	this->vessel_x = vessel_x;
	this->vessel_y = vessel_y;

	if ((outline != nullptr) && (this->preview_config != nullptr)) {
		if (this->outline == nullptr) {
			this->outline = new Outline(outline);
		} else if ((this->outline->center_foot.x != outline->center_foot.x) || (this->outline->center_foot.y != outline->center_foot.y)) {
			this->outline->clone_from(outline);
		}
	}
}

void DragTracklet::on_appdata(Uri^ profile, DragTrack^ profile_config) {
	this->profile_config = profile_config;

	// avoid updating raw instance accidently
	this->preview_config = ref new DragTrack(this->profile_config);
}

bool DragTracklet::ready() {
	return (this->preview_config != nullptr);
}

void DragTracklet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOX(w, this->width);
	SET_BOX(h, this->height);
}

void DragTracklet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
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

	if (this->profile != nullptr) {
		//ds->DrawGeometry(this->profile, cx, y, this->style.profile_color, this->style.profile_thickness, this->style.profile_style);
	}

	if ((this->vessel != nullptr) && (this->vessel->ready())) {
		//this->vessel->draw_profile(ds, this, cx, y, half_width, this->height);
	}

	//ds->DrawRectangle(x + border_off, y + border_off,
		//this->width - this->style.border_thickness, this->height - this->style.border_thickness,
		//this->style.border_color, this->style.border_thickness);
}

/*************************************************************************************************/
DragTrack^ DragTracklet::clone_profile(DragTrack^ dest, bool real_profile) {
	DragTrack^ clone = ((dest == nullptr) ? ref new DragTrack() : dest);

	clone->refresh(real_profile ? this->profile_config : this->preview_config);

	return clone;
}

void DragTracklet::preview(DragTrack^ src) {
	if (src == nullptr) {
		this->preview_config->refresh(this->profile_config);
	} else if (this->preview_config == nullptr) {
		this->preview_config = ref new DragTrack(src);
	} else {
		this->preview_config->refresh(src);
	}

	this->notify_updated();
}

void DragTracklet::refresh(DragTrack^ src) {
	this->store(this->ms_appdata_config, src);
}

/*************************************************************************************************/
DragTrack^ DragTrack::load(Platform::String^ path) {
	DragTrack^ dt = nullptr;
	size_t ptsize = sizeof(double2);
	Platform::String^ wtype;
	std::filebuf src;

	if (open_input_binary(src, path)) {
		dt = ref new DragTrack();
		wtype = read_wtext(src);
		discard_this_line(src);

		if (DT::DragheadTrack.ToString()->Equals(wtype)) {
			while (peek_char(src) != EOF) {
				wtype = read_wtext(src, char_end_of_word);

				if (DT::Period.ToString()->Equals(wtype)) {
					dt->begin_timepoint = read_natural(src);
					dt->begin_timepoint = read_natural(src);
				} else if (DT::AfterImagePeriod.ToString()->Equals(wtype)) {
					dt->after_image_period = read_natural(src);
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

bool DragTrack::save(DragTrack^ self, Platform::String^ path) {
	std::wofstream v_config;
	bool okay = false;

	if (open_output_binary(v_config, path)) {
		write_wtext(v_config, DT::DragheadTrack, true);

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

DragTrack::DragTrack(DragTrack^ src) {
	this->refresh(src);
}

void DragTrack::refresh(DragTrack^ src) {
	if ((src != nullptr) && (this != src)) {
		this->begin_timepoint = src->begin_timepoint;
		this->end_timepoint = src->end_timepoint;
		this->after_image_period = src->after_image_period;
		
		this->depth0 = src->depth0;
		this->interval = src->interval;

		this->track_width = src->track_width;
		this->track_color = src->track_color;
	}
}
