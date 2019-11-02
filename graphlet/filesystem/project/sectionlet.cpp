#include "graphlet/filesystem/project/sectionlet.hpp"

#include "datum/file.hpp"

#include "geometry.hpp"
#include "brushes.hxx"
#include "shape.hpp"
#include "paint.hpp"
#include "math.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

namespace {
	private enum class TS { TransverseSection, Region, ColorPlot };
}

/*************************************************************************************************/
FrontalSectionlet::FrontalSectionlet(SecDoc^ sec, bool draw_slope_lines, float thickness, CanvasSolidColorBrush^ cl_color, CanvasSolidColorBrush^ sl_color)
	: doc_sec(sec), master(nullptr), thickness(thickness), centerline_color(cl_color), sideline_color(sl_color), draw_slope_lines(draw_slope_lines) {
	this->enable_resizing(false);
	this->camouflage(true);

	CAS_SLOT(this->centerline_color, Colours::Red);
	CAS_SLOT(this->sideline_color, Colours::SpringGreen);
}

void FrontalSectionlet::construct() {
	this->slope_style = make_dash_stroke(CanvasDashStyle::Dash);

	if ((this->doc_sec != nullptr) && (this->doc_sec->centerline.size() > 1)) {
		SectionDot cl0 = this->doc_sec->centerline[0];
		SectionDot cl1 = this->doc_sec->centerline[1];
		double3 pt0, pt1;

		for (auto slit = this->doc_sec->sidelines.begin(); slit != this->doc_sec->sidelines.end(); slit++) {
			std::deque<std::pair<double3, double3>> segment;
			size_t count = slit->size();
			
			if (count > 1) {
				SectionDot* slope = &(*slit)[0];
				double position_sign = flsign(cross_product(slope->x - cl0.x, slope->y - cl0.y, cl1.x - cl0.x, cl1.y - cl0.y));
				
				for (size_t idx = 1; idx < count; idx++) {
					SectionDot* self = &(*slit)[idx];

					// only `flisnan(pt0.x)` works later.
					pt0 = double3(flnan, flnan, slope->slope_depth);
					pt1 = double3(flnan, flnan, self->slope_depth);
					
					if (self->grade > 0.0) {
						parallel_segment(slope->x, slope->y, self->x, self->y,
							(self->depth - self->slope_depth) * self->grade * position_sign,
							&pt0.x, &pt0.y, &pt1.x, &pt1.y);	
					}

					segment.push_back(std::pair<double3, double3>(pt0, pt1));
					slope = self;
				}
			}

			this->slope_segments.push_back(segment);
		}
	}
}

void FrontalSectionlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->available_visible_width(x));
	SET_BOX(height, this->available_visible_height(y));
}

void FrontalSectionlet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if ((this->master != nullptr) && (this->doc_sec != nullptr)) {
		float rx = x + Width;
		float by = y + Height;
		
		{ // draw centerline
			size_t count = this->doc_sec->centerline.size();

			if (count > 1) {
				float2 last_dot = this->master->position_to_local(this->doc_sec->centerline[0].x, this->doc_sec->centerline[0].y);

				for (size_t idx = 1; idx < count; idx++) {
					float2 this_dot = this->master->position_to_local(this->doc_sec->centerline[idx].x, this->doc_sec->centerline[idx].y);

					ds->DrawLine(last_dot, this_dot, this->centerline_color, this->thickness);
					last_dot = this_dot;
				}
			}
		}
		
		{ // draw sidelines and slope lines as well
			size_t n = this->doc_sec->sidelines.size();

			for (size_t idx = 0; idx < n; idx++) {
				auto sideline = this->doc_sec->sidelines[idx];
				auto segments = this->slope_segments[idx];
				size_t count = sideline.size();

				if (count > 1) {
					float2 last_dot = this->master->position_to_local(sideline[0].x, sideline[0].y);
					
					for (size_t dot = 1; dot < count; dot++) {
						float2 this_dot = this->master->position_to_local(sideline[dot].x, sideline[dot].y);

						if (this->draw_slope_lines) {
							double3 dot0 = segments[dot - 1].first;
							double3 dot1 = segments[dot - 1].second;

							if (!flisnan(dot0.x)) {
								float2 seg_dot0 = this->master->position_to_local(dot0.x, dot0.y);
								float2 seg_dot1 = this->master->position_to_local(dot1.x, dot1.y);

								ds->DrawLine(seg_dot0, seg_dot1, this->sideline_color, this->thickness, this->slope_style);
								ds->DrawLine(last_dot, seg_dot0, this->sideline_color, this->thickness, this->slope_style);
								ds->DrawLine(this_dot, seg_dot1, this->sideline_color, this->thickness, this->slope_style);
							}
						}

						ds->DrawLine(last_dot, this_dot, this->sideline_color, this->thickness);
						last_dot = this_dot;
					}
				}
			}
		}
	}
}

void FrontalSectionlet::attach_to_map(DigMaplet* master, bool force) {
	if (master != nullptr) {
		if (force || (this->master != master)) {
			this->notify_updated();
		}
	}

	this->master = master;
}
/*************************************************************************************************/
TransverseSectionlet::TransverseSectionlet(Platform::String^ section, float width, float height, Platform::String^ ext, Platform::String^ rootdir)
	: width(width), height(height) {
	if (section != nullptr) {
		this->ms_appdata_config = ms_appdata_file(section, ext, rootdir);
	} else {
		// TODO: meanwhile it's useless and easy to be used incorrectly
		this->ms_appdata_config = ref new Uri(ms_apptemp_file("section", ext));
	}

	if (this->height == 0.0F) {
		this->height = 200.0F;
	} else if (this->height < 0.0F) {
		this->height *= -this->width;
	}
}

TransverseSectionlet::~TransverseSectionlet() {
	this->unload(this->ms_appdata_config);
}

void TransverseSectionlet::construct() {
	this->load(this->ms_appdata_config);
	this->font = make_bold_text_format("Arial", 12.0F);
}

void TransverseSectionlet::on_appdata(Uri^ section, TransverseSection^ section_config) {
	this->section_config = section_config;

	// avoid updating raw instance accidently
	this->preview_config = ref new TransverseSection(this->section_config);
}

bool TransverseSectionlet::ready() {
	return (this->preview_config != nullptr);
}

void TransverseSectionlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOX(w, this->width);
	SET_BOX(h, this->height);
}

void TransverseSectionlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->preview_config != nullptr) {
	}

	ds->DrawRectangle(x + 0.5F, y + 0.5F, this->width - 1.0F, this->height - 1.0F, Colours::GrayText);
}

/*************************************************************************************************/
TransverseSection^ TransverseSectionlet::clone_section(TransverseSection^ dest, bool real_section) {
	TransverseSection^ clone = ((dest == nullptr) ? ref new TransverseSection() : dest);

	clone->refresh(real_section ? this->section_config : this->preview_config);

	return clone;
}

void TransverseSectionlet::preview(TransverseSection^ src) {
	if (src == nullptr) {
		this->preview_config->refresh(this->section_config);
	} else if (this->preview_config == nullptr) {
		this->preview_config = ref new TransverseSection(src);
	} else {
		this->preview_config->refresh(src);
	}

	this->notify_updated();
}

void TransverseSectionlet::refresh(TransverseSection^ src) {
	this->store(this->ms_appdata_config, src);
}

/*************************************************************************************************/
TransverseSection^ TransverseSection::load(Platform::String^ path) {
	TransverseSection^ cs = nullptr;
	size_t ptsize = sizeof(double2);
	Platform::String^ wtype;
	std::filebuf src;

	if (open_input_binary(src, path)) {
		cs = ref new TransverseSection();
		wtype = read_wtext(src);
		discard_this_line(src);

		if (TS::TransverseSection.ToString()->Equals(wtype)) {
			while (peek_char(src) != EOF) {
				wtype = read_wtext(src, char_end_of_word);

				if (TS::Region.ToString()->Equals(wtype)) {
					cs->width = read_flonum(src);
					cs->min_depth = read_flonum(src);
					cs->max_depth = read_flonum(src);
				} else if (TS::ColorPlot.ToString()->Equals(wtype)) {
					cs->depth_distance = read_flonum(src);
					cs->dragheads_distance = read_flonum(src);
				}

				discard_this_line(src);
			}
		}
	}

	return cs;
}

bool TransverseSection::save(TransverseSection^ self, Platform::String^ path) {
	std::wofstream v_config;
	bool okay = false;

	if (open_output_binary(v_config, path)) {
		write_wtext(v_config, TS::TransverseSection, true);

		write_wtext(v_config, TS::Region);
		v_config << " " << self->width << " " << self->min_depth << " " << self->max_depth;
		write_newline(v_config);

		write_wtext(v_config, TS::ColorPlot);
		v_config << " " << self->depth_distance << " " << self->dragheads_distance;
		write_newline(v_config);

		v_config.flush();

		okay = true;
	}

	return okay;
}

TransverseSection::TransverseSection(TransverseSection^ src) {
	this->refresh(src);
}

void TransverseSection::refresh(TransverseSection^ src) {
	if ((src != nullptr) && (this != src)) {
		this->width = src->width;
		this->min_depth = src->min_depth;
		this->max_depth = src->max_depth;
		this->depth_distance = src->depth_distance;
		this->dragheads_distance = src->dragheads_distance;
	}
}
