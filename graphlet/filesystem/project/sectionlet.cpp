#include "graphlet/filesystem/project/sectionlet.hpp"

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

static Microsoft::Graphics::Canvas::Geometry::CanvasStrokeStyle^ default_slope_style = make_dash_stroke(CanvasDashStyle::Dash);

/*************************************************************************************************/
Sectionlet::Entity::Entity(SecDoc^ sec) : doc_sec(sec), plane(nullptr) {
	this->ps_boundry.x = flnan;
	this->sb_boundry.x = flnan;
}

Sectionlet::Entity::~Entity() {
	if (this->plane != nullptr) {
		delete this->plane;
	}
}

void Sectionlet::Entity::construct() {
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

		this->plane = new Outline(intersection_count, interslope_count);
	}
}

void Sectionlet::Entity::draw(DigMaplet* master_map, Sectionlet::Style* style, CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->doc_sec != nullptr) {
		float rx = x + Width;
		float by = y + Height;

		{ // draw centerline
			size_t count = this->doc_sec->centerline.size();

			if (count >= 2) {
				float2 last_dot = master_map->position_to_local(this->doc_sec->centerline[0].x, this->doc_sec->centerline[0].y, x, y);

				for (size_t idx = 1; idx < count; idx++) {
					float2 this_dot = master_map->position_to_local(this->doc_sec->centerline[idx].x, this->doc_sec->centerline[idx].y, x, y);

					ds->DrawLine(last_dot, this_dot, style->centerline_color, style->thickness);
					last_dot = this_dot;
				}
			}
		}

		for (size_t idx = 0; idx < this->doc_sec->sidelines.size(); idx++) {
			auto sideline = this->doc_sec->sidelines[idx];
			auto segments = this->slope_segments[idx];
			size_t count = sideline.size();

			if (count > 1) {
				float2 last_dot = master_map->position_to_local(sideline[0].x, sideline[0].y, x, y);

				for (size_t dot = 1; dot < count; dot++) {
					float2 this_dot = master_map->position_to_local(sideline[dot].x, sideline[dot].y, x, y);

					if (style->draw_slope_lines) {
						double3 dot0 = segments[dot - 1].first;
						double3 dot1 = segments[dot - 1].second;

						if (!flisnan(dot0.x)) {
							float2 seg_dot0 = master_map->position_to_local(dot0.x, dot0.y, x, y);
							float2 seg_dot1 = master_map->position_to_local(dot1.x, dot1.y, x, y);

							ds->DrawLine(seg_dot0, seg_dot1, style->sideline_color, style->thickness, style->slope_style);
							ds->DrawLine(last_dot, seg_dot0, style->sideline_color, style->thickness, style->slope_style);
							ds->DrawLine(this_dot, seg_dot1, style->sideline_color, style->thickness, style->slope_style);
						}
					}

					ds->DrawLine(last_dot, this_dot, style->sideline_color, style->thickness);
					last_dot = this_dot;
				}
			}
		}
	}

	if ((this->plane != nullptr) && (!flisnan(this->plane->center_foot.x))) {
		float2 ray = master_map->position_to_local(this->plane->center_foot.x, this->plane->center_foot.y, x, y);

		if (!flisnan(this->ps_boundry.x)) {
			ds->DrawLine(ray, master_map->position_to_local(this->ps_boundry.x, this->ps_boundry.y, x, y),
				style->section_color, style->thickness, style->slope_style);
		}

		if (!flisnan(this->sb_boundry.x)) {
			ds->DrawLine(ray, master_map->position_to_local(this->sb_boundry.x, this->sb_boundry.y, x, y),
				style->section_color, style->thickness, style->slope_style);
		}
	}
}

const Outline* Sectionlet::Entity::section(double x, double y, bool need_slopes) {
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

					this->section(x, y, foot_x, foot_y, need_slopes);

					break;
				}
			}
		}
	}

	return this->plane;
}

void Sectionlet::Entity::section(double x, double y, double center_x, double center_y, bool need_slopes) {
	ProfileDot* self = nullptr;
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

						if (need_slopes && (!flisnan(seg_dot0->x))) {
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
Sectionlet::Sectionlet(SecDoc^ sec, bool draw_slope_lines, float thickness
	, CanvasSolidColorBrush^ cl_color, CanvasSolidColorBrush^ sl_color, CanvasSolidColorBrush^ sec_color) {
	this->enable_resizing(false);
	this->camouflage(true);

	this->sections.push_back(Sectionlet::Entity(sec));

	CAS_SLOT(this->style.centerline_color, Colours::Red);
	CAS_SLOT(this->style.sideline_color, Colours::SpringGreen);
	CAS_SLOT(this->style.section_color, this->style.sideline_color);
	
	this->style.thickness = thickness;
	this->style.draw_slope_lines = draw_slope_lines;
}

void Sectionlet::construct() {
	this->style.slope_style = default_slope_style;
	
	for (auto it = this->sections.begin(); it != this->sections.end(); it++) {
		(*it).construct();
	}
}

void Sectionlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->available_visible_width(x));
	SET_BOX(height, this->available_visible_height(y));
}

void Sectionlet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if (this->master_map != nullptr) {
		for (auto it = this->sections.begin(); it != this->sections.end(); it++) {
			(*it).draw(this->master_map, &this->style, ds, x, y, Width, Height);
		}
	}
}

const Outline* Sectionlet::section(double x, double y) {
	const Outline* plane = nullptr;

	for (auto it = this->sections.begin(); it != this->sections.end(); it++) {
		const Outline* p = (*it).section(x, y, this->style.draw_slope_lines);

		if (!flisnan(p->center_foot.x)) {
			plane = p;
			break;
		}
	}

	return plane;
}

void Sectionlet::merge(SecDoc^ sec) {
	if (sec != nullptr) {
		Sectionlet::Entity entity(sec);

		entity.construct();
		this->sections.push_back(entity);
		this->notify_updated();
	}
}
