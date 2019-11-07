#include "graphlet/filesystem/project/sectionlet.hpp"

#include "datum/file.hpp"
#include "datum/fixnum.hpp"

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

	private struct PlaneDot {
		double x;
		double y;
		double distance;
		double depth;
	};
}

static Microsoft::Graphics::Canvas::Text::CanvasTextFormat^ default_mark_font = make_bold_text_format("Microsoft Yahei", 10.0F);
static Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ default_slop_style = make_dash_stroke(CanvasDashStyle::Dash);

static void prepare_transverse_section_style(TransverseSectionStyle* style) {
	CAS_SLOT(style->font, default_mark_font);
	CAS_SLOT(style->ps_draghead_color, Colours::Red);
	CAS_SLOT(style->sb_draghead_color, Colours::Green);

	CAS_SLOT(style->section_color, Colours::DarkKhaki);
	CAS_SLOT(style->centerline_color, Colours::Crimson);
	CAS_SLOT(style->centerline_style, default_slop_style);
	CAS_SLOT(style->haxes_color, Colours::Tomato);
	CAS_SLOT(style->haxes_style, default_slop_style);
	CAS_SLOT(style->vaxes_color, Colours::DodgerBlue);
	CAS_SLOT(style->vaxes_style, default_slop_style);
	CAS_SLOT(style->border_color, Colours::GrayText);

	FLCAS_SLOT(style->section_thickness, 1.5F);
	FLCAS_SLOT(style->border_thickness, 1.5F);
	FLCAS_SLOT(style->centerline_thickness, 1.0F);
	FLCAS_SLOT(style->haxes_thickness, 0.5F);
	FLCAS_SLOT(style->vaxes_thickness, 0.5F);

	ICAS_SLOT(style->haxes_count, 4);
	ICAS_SLOT(style->vaxes_half_count, 4);
}

TransverseSectionStyle WarGrey::SCADA::default_transverse_section_style(CanvasSolidColorBrush^ section, CanvasSolidColorBrush^ ps_color, CanvasSolidColorBrush^ sb_color) {
	TransverseSectionStyle style;

	style.section_color = section;
	style.section_style = nullptr;
	style.ps_draghead_color = ps_color;
	style.sb_draghead_color = sb_color;

	prepare_transverse_section_style(&style);

	return style;
}

private struct WarGrey::SCADA::TransversePlane {
public:
	~TransversePlane() noexcept {
		if (this->sides != nullptr) {
			delete[] this->sides;
		}

		if (this->slopes != nullptr) {
			delete[] this->slopes;
		}
	}

	TransversePlane(int side_count, int slope_count) : side_count(side_count), slope_count(slope_count) {
		this->center_foot.x = flnan;

		if (this->side_count > 0) {
			this->sides = new PlaneDot[this->side_count];
		}

		if (this->slope_count > 0) {
			this->slopes = new PlaneDot[this->slope_count];
		}
	}

	TransversePlane(const TransversePlane* src) : TransversePlane(src->side_count, src->slope_count) {
		this->clone_from(src);
	}


public:
	void clone_from(const TransversePlane* src) {
		int side_mcount = fxmin(this->side_count, src->side_count);
		int slope_mcount = fxmin(this->slope_count, src->slope_count);

		this->center_foot = src->center_foot;
		this->center_origin = src->center_origin;

		for (int idx = 0; idx < side_mcount; idx++) {
			this->sides[idx] = src->sides[idx];
		}

		for (int idx = 0; idx < slope_mcount; idx++) {
			this->slopes[idx] = src->slopes[idx];
		}
	}

public:
	WarGrey::SCADA::double3 center_foot;
	WarGrey::SCADA::double2 center_origin;
	::PlaneDot* sides;
	::PlaneDot* slopes;
	int side_count;
	int slope_count;
};

/*************************************************************************************************/
FrontalSectionlet::FrontalSectionlet(SecDoc^ sec, bool draw_slope_lines, float thickness
	, CanvasSolidColorBrush^ cl_color, CanvasSolidColorBrush^ sl_color, CanvasSolidColorBrush^ sec_color)
	: doc_sec(sec), master(nullptr), thickness(thickness), draw_slope_lines(draw_slope_lines), plane(nullptr)
	, centerline_color(cl_color), sideline_color(sl_color), section_color(sec_color) {
	this->enable_resizing(false);
	this->camouflage(true);

	CAS_SLOT(this->centerline_color, Colours::Red);
	CAS_SLOT(this->sideline_color, Colours::SpringGreen);
	CAS_SLOT(this->section_color, this->sideline_color);

	this->ps_boundry.x = flnan;
	this->sb_boundry.x = flnan;
}

FrontalSectionlet::~FrontalSectionlet() {
	if (this->plane != nullptr) {
		delete this->plane;
	}
}

void FrontalSectionlet::construct() {
	this->slope_style = default_slop_style;

	if ((this->doc_sec != nullptr) && (this->doc_sec->centerline.size() > 1)) {
		SectionDot cl0 = this->doc_sec->centerline[0];
		SectionDot cl1 = this->doc_sec->centerline[1];
		int interslope_count = 0;
		int intersection_count = 0;
		double3 pt0, pt1;

		for (auto slit = this->doc_sec->sidelines.begin(); slit != this->doc_sec->sidelines.end(); slit++) {
			std::deque<std::pair<double3, double3>> segment;
			size_t count = slit->size();
			
			if (count > 1) {
				SectionDot* slope = &(*slit)[0];
				bool has_slope = false;

				for (size_t idx = 1; idx < count; idx++) {
					SectionDot* self = &(*slit)[idx];

					// only `flisnan(pt0.x)` works later.
					pt0 = double3(flnan, flnan, slope->slope_depth);
					pt1 = double3(flnan, flnan, self->slope_depth);
					
					if (self->grade > 0.0) {
						parallel_segment(slope->x, slope->y, self->x, self->y,
							(self->depth - self->slope_depth) * self->grade * self->position_sign,
							&pt0.x, &pt0.y, &pt1.x, &pt1.y);

						has_slope = true;
					}

					segment.push_back(std::pair<double3, double3>(pt0, pt1));
					slope = self;
				}

				intersection_count += 1;

				if (has_slope) {
					interslope_count += 1;
				}
			}

			this->slope_segments.push_back(segment);
		}

		this->plane = new TransversePlane(intersection_count, interslope_count);
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

			if (count >= 2) {
				float2 last_dot = this->master->position_to_local(this->doc_sec->centerline[0].x, this->doc_sec->centerline[0].y, x, y);

				for (size_t idx = 1; idx < count; idx++) {
					float2 this_dot = this->master->position_to_local(this->doc_sec->centerline[idx].x, this->doc_sec->centerline[idx].y, x, y);

					ds->DrawLine(last_dot, this_dot, this->centerline_color, this->thickness);
					last_dot = this_dot;
				}
			}
		}

		for (size_t idx = 0; idx < this->doc_sec->sidelines.size(); idx++) {
			auto sideline = this->doc_sec->sidelines[idx];
			auto segments = this->slope_segments[idx];
			size_t count = sideline.size();

			if (count > 1) {
				float2 last_dot = this->master->position_to_local(sideline[0].x, sideline[0].y, x, y);

				for (size_t dot = 1; dot < count; dot++) {
					float2 this_dot = this->master->position_to_local(sideline[dot].x, sideline[dot].y, x, y);

					if (this->draw_slope_lines) {
						double3 dot0 = segments[dot - 1].first;
						double3 dot1 = segments[dot - 1].second;

						if (!flisnan(dot0.x)) {
							float2 seg_dot0 = this->master->position_to_local(dot0.x, dot0.y, x, y);
							float2 seg_dot1 = this->master->position_to_local(dot1.x, dot1.y, x, y);

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

	if ((this->plane != nullptr) && (!flisnan(this->plane->center_foot.x))) {
		float2 ray = this->master->position_to_local(this->plane->center_foot.x, this->plane->center_foot.y, x, y);

		if (!flisnan(this->ps_boundry.x)) {
			ds->DrawLine(ray, this->master->position_to_local(this->ps_boundry.x, this->ps_boundry.y, x, y),
				this->section_color, this->thickness, this->slope_style);
		}

		if (!flisnan(this->sb_boundry.x)) {
			ds->DrawLine(ray, this->master->position_to_local(this->sb_boundry.x, this->sb_boundry.y, x, y),
				this->section_color, this->thickness, this->slope_style);
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

const TransversePlane* FrontalSectionlet::section(double x, double y) {
	this->ps_boundry.x = flnan;
	this->sb_boundry.x = flnan;
	
	if ((this->doc_sec != nullptr) && (this->plane != nullptr)) {
		size_t count = this->doc_sec->centerline.size();
	
		this->plane->center_foot.x = flnan;

		if (count > 1) {
			double foot_x, foot_y;
			
			for (size_t idx = 1; idx < count; idx++) {
				double x1 = this->doc_sec->centerline[idx - 1].x;
				double y1 = this->doc_sec->centerline[idx - 1].y;
				double z1 = this->doc_sec->centerline[idx - 1].depth;
				double x2 = this->doc_sec->centerline[idx].x;
				double y2 = this->doc_sec->centerline[idx].y;
				double z2 = this->doc_sec->centerline[idx].depth;

				// NOTE: the turning points on centerline are guaranteed to be unique when loading them
				if (is_foot_on_segment(x, y, x1, y1, x2, y2)) {
					point_foot_on_segment(x, y, x1, y1, x2, y2, &foot_x, &foot_y);

					{ // resolve depth
						double t = flsqrt(points_distance_squared(x1, y1, foot_x, foot_y) / points_distance_squared(x1, y1, x2, y2));

						this->plane->center_foot = double3(foot_x, foot_y, (z2 - z1) * t + z1);
						this->plane->center_origin = double2(x1, y1);
					}

					if (points_distance_squared(x, y, foot_x, foot_y) < 0.01) {
						line_normal0_vector(x1, y1, x2, y2, 1.0, &x, &y, foot_x, foot_y);
					}

					this->section(x, y, foot_x, foot_y);
					
					break;
				}
			}
		}
	}

	return this->plane;
}

void FrontalSectionlet::section(double x, double y, double center_x, double center_y) {
	PlaneDot* self = nullptr;
	double ps_distance = 0.0;
	double sb_distance = 0.0;
	int section_idx = 0;
	int slope_idx = 0;
	double sec_x = 0.0;
	double sec_y = 0.0;

	for (size_t idx = 0; idx < this->doc_sec->sidelines.size(); idx++) {
		auto sideline = this->doc_sec->sidelines[idx];
		auto segments = this->slope_segments[idx];
		size_t count = sideline.size();
		double ray_t = flnan;
		double segment_t = flnan;

		if (count > 1) {
			SectionDot* last_dot = &sideline[0];
			
			self = &this->plane->sides[section_idx];
			self->x = flnan;

			for (size_t dot = 1; dot < count; dot++) {
				SectionDot* this_dot = &sideline[dot];
				
				if (lines_intersection(x, y, center_x, center_y, last_dot->x, last_dot->y, this_dot->x, this_dot->y, &sec_x, &sec_y, &ray_t, &segment_t)) {
					if (flin(0.0, segment_t, 1.0)) {
						double3* seg_dot0 = &segments[dot - 1].first;
						double3* seg_dot1 = &segments[dot - 1].second;
	
						self->x = sec_x;
						self->y = sec_y;
						self->distance = points_distance(center_x, center_y, self->x, self->y) * last_dot->position_sign;
						self->depth = (last_dot->depth - this_dot->depth) * segment_t + this_dot->depth;

						if (this->draw_slope_lines && (!flisnan(seg_dot0->x))) {
							self = &this->plane->slopes[slope_idx];
							self->x = flnan;

							if (lines_intersection(x, y, center_x, center_y, seg_dot0->x, seg_dot0->y, seg_dot1->x, seg_dot1->y, &sec_x, &sec_y, &ray_t, &segment_t)) {
								if (flin(0.0, segment_t, 1.0)) {
									self->x = sec_x;
									self->y = sec_y;
									self->distance = points_distance(center_x, center_y, self->x, self->y) * last_dot->position_sign;
									self->depth = (seg_dot0->z - seg_dot1->z) * segment_t + seg_dot1->z;
									slope_idx += 1;
								}
							}
						}

						if (self->distance >= 0.0) {
							if (self->distance > ps_distance) {
								this->ps_boundry.x = self->x;
								this->ps_boundry.y = self->y;
								ps_distance = self->distance;
							}
						} else {
							if (self->distance < sb_distance) {
								this->sb_boundry.x = self->x;
								this->sb_boundry.y = self->y;
								sb_distance = self->distance;
							}
						}

						break;
					}
				}

				last_dot = this_dot;
			}

			section_idx += 1;
		}
	}
}

/*************************************************************************************************/
TransverseSectionlet::TransverseSectionlet(Platform::String^ section, float width, float height, Platform::String^ ext, Platform::String^ rootdir)
	: TransverseSectionlet(default_transverse_section_style(), section, width, height, ext, rootdir) {}

TransverseSectionlet::TransverseSectionlet(TransverseSectionStyle& style, Platform::String^ section, float width, float height, Platform::String^ ext, Platform::String^ rootdir)
	: width(width), height(height), style(style) {
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

	prepare_transverse_section_style(&this->style);
}

TransverseSectionlet::~TransverseSectionlet() {
	this->unload(this->ms_appdata_config);

	if (this->plane) {
		delete this->plane;
	}
}

void TransverseSectionlet::construct() {
	this->load(this->ms_appdata_config);
}

void TransverseSectionlet::update_section(const TransversePlane* plane) {
	if (plane != nullptr) {
		if (this->plane == nullptr) {
			this->plane = new TransversePlane(plane);
			this->update_vertical_axes();
		} else if ((this->plane->center_foot.x != plane->center_foot.x) || (this->plane->center_foot.y != plane->center_foot.y)) {
			this->plane->clone_from(plane);
			this->update_vertical_axes();
		}
	}
}

void TransverseSectionlet::on_appdata(Uri^ section, TransverseSection^ section_config) {
	this->section_config = section_config;

	// avoid updating raw instance accidently
	this->preview_config = ref new TransverseSection(this->section_config);

	this->update_horizontal_axes();
	this->update_vertical_axes();
}

bool TransverseSectionlet::ready() {
	return (this->preview_config != nullptr);
}

void TransverseSectionlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOX(w, this->width);
	SET_BOX(h, this->height);
}

void TransverseSectionlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float border_off = this->style.border_thickness * 0.5F;
	float cx = x + this->width * 0.5F;

	if (this->preview_config != nullptr) {
		ds->DrawCachedGeometry(this->haxes, x, y, this->style.haxes_color);
		ds->DrawCachedGeometry(this->vaxes, x, y, this->style.vaxes_color);

		ds->DrawCachedGeometry(this->hmarks, x, y, this->style.haxes_color);
		ds->DrawCachedGeometry(this->vmarks, x, y, this->style.vaxes_color);
	}

	ds->DrawLine(cx, y, cx, y + this->height, this->style.centerline_color, this->style.centerline_thickness, this->style.centerline_style);

	if ((this->preview_config != nullptr) && (this->plane != nullptr)) {
		double depth_range = (this->preview_config->max_depth - this->preview_config->min_depth);
		double xscale = this->width / this->preview_config->width;
		double yscale = ((depth_range > 0.0) ? this->height / depth_range : 1.0);

		for (int idx = 0; idx < this->plane->side_count; idx++) {
			float dx = cx - float(this->plane->sides[idx].distance * xscale);
			float dy = y + float((this->plane->sides[idx].depth - this->preview_config->min_depth) * yscale);

			ds->DrawCircle(dx, dy, 2.0F, this->style.section_color, this->style.section_thickness);
		}
	}

	ds->DrawRectangle(x + border_off, y + border_off,
		this->width - this->style.border_thickness, this->height - this->style.border_thickness,
		this->style.border_color, this->style.border_thickness);
}

void TransverseSectionlet::update_horizontal_axes() {
	CanvasGeometry^ marks = blank();
	CanvasPathBuilder^ axes = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float interval = this->height / float(this->style.haxes_count + 1);
	double delta = (this->preview_config->max_depth - this->preview_config->min_depth) / double(this->style.haxes_count + 1);
	float y = this->height - style.haxes_thickness * 0.5F;
	TextExtent mark_te;

	for (int i = 1; i <= this->style.haxes_count; i++) {
		float ythis = y - interval * float(i);
		Platform::String^ mark = flstring(this->preview_config->max_depth - delta * double(i), 1U);
		CanvasGeometry^ gmark = paragraph(mark, this->style.font, &mark_te);

		marks = geometry_union(marks, gmark, style.border_thickness + mark_te.height * 0.618F, ythis - mark_te.height);

		axes->BeginFigure(0.0F, ythis);
		axes->AddLine(this->width, ythis);
		axes->EndFigure(CanvasFigureLoop::Open);
	}

	this->hmarks = geometry_freeze(marks);
	this->haxes = geometry_freeze(geometry_stroke(CanvasGeometry::CreatePath(axes), style.haxes_thickness, style.haxes_style));
}

void TransverseSectionlet::update_vertical_axes() {
	CanvasPathBuilder^ axes = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	CanvasGeometry^ marks = blank();
	int count = this->style.vaxes_half_count * 2;
	float interval = this->width / float(count + 2);
	float cx = this->width * 0.5F;
	double delta = this->preview_config->width / double(count + 2);
	double start = -this->preview_config->width * 0.5;
	float x = style.haxes_thickness * 0.5F;
	float y = this->height - style.border_thickness;
	CanvasGeometry^ xmark;
	CanvasGeometry^ ymark;
	TextExtent x_te, y_te;

	for (int i = 0; i <= count + 2; i++) {
		float xthis = x + interval * float(i);
		double distance = start + delta * double(i);

		if ((this->plane != nullptr) && (!flisnan(this->plane->center_foot.x))) {
			double2 dot;

			line_normal0_vector(this->plane->center_foot.x, this->plane->center_foot.y, this->plane->center_origin.x, this->plane->center_origin.y,
				-distance, &dot.x, &dot.y, this->plane->center_foot.x, this->plane->center_foot.y);

			xmark = paragraph("X:" + flstring(dot.x, 1), this->style.font, &x_te);
			ymark = paragraph("Y:" + flstring(dot.y, 1), this->style.font, &y_te);
		} else {
			xmark = paragraph(flstring(distance, 1), this->style.font, &x_te);
			ymark = nullptr;
		}

		if (i != this->style.vaxes_half_count + 1) {
			axes->BeginFigure(xthis, 0.0F);
			axes->AddLine(xthis, this->height);
			axes->EndFigure(CanvasFigureLoop::Open);
		}

		if (ymark == nullptr) {
			marks = geometry_union(marks, xmark, xthis - x_te.width * 0.5F, y - x_te.height);
		} else {
			marks = geometry_union(marks, xmark, xthis - x_te.width * 0.5F, y - x_te.height - y_te.height);
			marks = geometry_union(marks, ymark, xthis - y_te.width * 0.5F, y - y_te.height);
		}
	}

	this->vmarks = geometry_freeze(marks);
	this->vaxes = geometry_freeze(geometry_stroke(CanvasGeometry::CreatePath(axes), style.haxes_thickness, style.haxes_style));
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
