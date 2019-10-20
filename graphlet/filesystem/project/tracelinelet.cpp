#include "graphlet/filesystem/project/tracelinelet.hpp"

#include "datum/string.hpp"
#include "datum/flonum.hpp"

#include "geometry.hpp"
#include "brushes.hxx"
#include "shape.hpp"
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

/*************************************************************************************************/
Tracelinelet::Tracelinelet(JobDoc^ jobs, float handler_size, CanvasSolidColorBrush^ color, CanvasSolidColorBrush^ hicolor
	, CanvasSolidColorBrush^ handler_color, CanvasSolidColorBrush^ handler_hicolor)
	: jobs_dat(jobs), master(nullptr), handler_half_size(handler_size * 0.5F)
	, color(color), hicolor(hicolor), handler_color(handler_color), handler_hicolor(handler_hicolor) {
	this->enable_resizing(false);
	this->enable_events(true, false);

	CAS_SLOT(this->color, Colours::RoyalBlue);
	CAS_SLOT(this->hicolor, Colours::Crimson);
	CAS_SLOT(this->handler_color, Colours::GhostWhite);
	CAS_SLOT(this->handler_hicolor, this->hicolor);

	if (this->handler_half_size <= 0.0F) {
		this->handler_half_size = 4.0F;
	}
}

void Tracelinelet::construct() {
	float handler_size = this->handler_half_size * 2.0F;

	this->handler = geometry_draft(rectangle(-this->handler_half_size, -this->handler_half_size, handler_size, handler_size), 2.0F);
}

void Tracelinelet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->available_visible_width(x));
	SET_BOX(height, this->available_visible_height(x));
}

bool Tracelinelet::is_colliding_with_mouse(float local_x, float local_y) {
	return (this->find_handler(local_x, local_y) >= 0);
}

void Tracelinelet::on_tap(float local_x, float local_y) {
	int job;
	int seq = this->find_handler(local_x, local_y, &job);

	// NOTE: the job and seq are guaranteed valid by `this::is_colliding_with_mouse()`

	if (this->jobs_dat->current_job != job) {
		this->jobs_dat->current_job = job;
		this->jobs_dat->current_section = seq;

		this->notify_updated();
	}
}

void Tracelinelet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if ((this->master != nullptr) && (this->jobs_dat != nullptr)) {
		float rx = x + Width;
		float by = y + Height;
		float handler_x, handler_y;

		for (auto jit = this->jobs_dat->jobs.begin(); jit != this->jobs_dat->jobs.end(); jit++) {
			auto sections = jit->second;
			
			for (auto it = sections.begin(); it != sections.end(); it++) {
				float2 spt = this->master->position_to_local(it->sx, it->sy, x, y);
				float2 ept = this->master->position_to_local(it->ex, it->ey, x, y);

				if (rectangle_overlay(x, y, rx, by, flmin(spt.x, ept.x), flmin(spt.y, ept.y), flmax(spt.x, ept.x), flmax(spt.y, ept.y))) {
					line_point(spt, ept, 0.5, &handler_x, &handler_y);

					if (this->jobs_dat->current_job == it->gid) {
						ds->DrawLine(spt, ept, this->hicolor);

						if (this->jobs_dat->current_section == it->seq) {
							ds->DrawCachedGeometry(this->handler, handler_x, handler_y, this->handler_hicolor);
						} else {
							ds->DrawCachedGeometry(this->handler, handler_x, handler_y, this->handler_color);
						}
					} else {
						ds->DrawLine(spt, ept, this->color);
						ds->DrawCachedGeometry(this->handler, handler_x, handler_y, this->handler_color);
					}
				}
			}
		}
	}
}

void Tracelinelet::attach_to_map(DigMaplet* master, bool force) {
	if (master != nullptr) {
		if (force || (this->master != master)) {	
			this->notify_updated();
		}
	}

	this->master = master;
}

void Tracelinelet::on_vessel_move(double vessel_x, double vessel_y) {
	if (this->jobs_dat != nullptr) {
		auto the_jobline = this->jobs_dat->jobs[this->jobs_dat->current_job];
		
		this->jobs_dat->current_section = -1;

		for (auto it = the_jobline.begin(); it != the_jobline.end(); it++) {
			if (is_foot_on_segment(vessel_x, vessel_y, it->sx, it->sy, it->ex, it->ey)) {
				this->jobs_dat->current_section = it->seq;
				break;
			}
		}
	}
}

int Tracelinelet::find_handler(float local_x, float local_y, int* group) {
	float handler_x, handler_y;
	float x, y;
	int seq = -1;

	if ((this->master != nullptr) && (this->jobs_dat != nullptr)) {
		this->fill_my_location(&x, &y);

		for (auto jit = this->jobs_dat->jobs.begin(); jit != this->jobs_dat->jobs.end(); jit++) {
			auto sections = jit->second;

			for (auto it = sections.begin(); it != sections.end(); it++) {
				float2 spt = this->master->position_to_local(it->sx, it->sy, x, y);
				float2 ept = this->master->position_to_local(it->ex, it->ey, x, y);

				line_point(spt, ept, 0.5, &handler_x, &handler_y);

				if (flin(handler_x - this->handler_half_size, local_x, handler_x + this->handler_half_size)
					&& flin(handler_y - this->handler_half_size, local_y, handler_y + this->handler_half_size)) {
					seq = it->seq;
					SET_BOX(group, it->gid);

					break;
				}
			}
		}
	}

	return seq;
}
