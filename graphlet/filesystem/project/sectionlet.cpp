#include "graphlet/filesystem/project/sectionlet.hpp"

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

/*************************************************************************************************/
Sectionlet::Sectionlet(SecDoc^ sec, bool draw_slope_lines, float thickness, CanvasSolidColorBrush^ cl_color, CanvasSolidColorBrush^ sl_color)
	: doc_sec(sec), master(nullptr), thickness(thickness), centerline_color(cl_color), sideline_color(sl_color), draw_slope_lines(draw_slope_lines) {
	this->enable_resizing(false);
	this->camouflage(true);

	CAS_SLOT(this->centerline_color, Colours::Red);
	CAS_SLOT(this->sideline_color, Colours::SpringGreen);
}

void Sectionlet::construct() {
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

void Sectionlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->available_visible_width(x));
	SET_BOX(height, this->available_visible_height(y));
}

void Sectionlet::draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
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

void Sectionlet::attach_to_map(DigMaplet* master, bool force) {
	if (master != nullptr) {
		if (force || (this->master != master)) {
			this->notify_updated();
		}
	}

	this->master = master;
}
