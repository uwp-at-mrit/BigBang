#include <algorithm>

#include "graphlet/filesystem/project/reader/secdoc.hxx"

#include "datum/file.hpp"
#include "datum/fixnum.hpp"

#include "math.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

/*************************************************************************************************/
SectionDot::SectionDot(double x, double y, double depth)
	: SectionDot(x, y, depth, 0.0, 0.0, 0.0) {}

SectionDot::SectionDot(double x, double y, double depth, double slope_depth, double grade, double posign)
	: x(x), y(y), depth(depth), slope_depth(slope_depth), grade(grade), position_sign(posign) {}

/*************************************************************************************************/
SecDoc::SecDoc(std::filebuf& sec) {
	double x, y, depth, slope_depth, grade;
	
	{ // read centerline dots
		long long dot_num = read_integer(sec);
		double last_x = flnan;
		double last_y = flnan;

		discard_this_line(sec);
		for (long long idx = 0; idx < dot_num; idx++) {
			if (peek_char(sec) == EOF) {
				break;
			} else {
				x = read_flonum(sec);
				read_char(sec);
				y = read_flonum(sec);
				read_char(sec);
				depth = read_flonum(sec);

				if (!((last_x == x) && (last_y == y))) {
					this->centerline.push_back(SectionDot(x, y, depth));
					
					last_x = x;
					last_y = y;
				}

				discard_this_line(sec);
			}
		}
	}

	{ // read side lines
		long long sideline_num = read_integer(sec);
		SectionDot cl0(0.0, 0.0, 0.0), cl1(1.0, 0.0, 0.0);
		auto minpos = this->sidelines.end();
		double min_distance2 = infinity;
		double distance2 = 0.0;
		
		if (this->centerline.size() >= 2) {
			cl0 = this->centerline[0];
			cl1 = this->centerline[1];
		}

		discard_this_line(sec);
		for (long long sidx = 0; sidx < sideline_num; sidx++) {
			std::deque<SectionDot> this_sideline;
			long long dot_num = read_integer(sec);
			double posign = 0.0;
			
			discard_this_line(sec);

			if (dot_num > 0) {
				for (long long idx = 0; idx < dot_num; idx++) {
					x = read_flonum(sec);
					read_char(sec);
					y = read_flonum(sec);
					read_char(sec);
					depth = read_flonum(sec);
					read_char(sec);
					slope_depth = read_flonum(sec);
					read_char(sec);
					grade = read_flonum(sec);
					read_char(sec);

					if (posign == 0.0) {
						posign = flsign(cross_product(x - cl0.x, y - cl0.y, cl1.x - cl0.x, cl1.y - cl0.y));
					}

					if (distance2 == 0.0) {
						distance2 = point_segment_distance_squared(x, y, cl0.x, cl0.y, cl1.x, cl1.y) * posign;
					}

					this_sideline.push_back(SectionDot(x, y, depth, slope_depth, grade, posign));

					discard_this_line(sec);
				}

				this->sidelines.push_back(this_sideline);
			}
		}
	}
}

/*************************************************************************************************/
static inline bool plane_dot_compare(ProfileDot& first, ProfileDot& second) {
	// NOTE: "being in order" is the only thing mattering regardless ascent or descent 
	return (first.distance < second.distance);
}

Outline::~Outline() noexcept {
	if (this->dots != nullptr) {
		delete[] this->dots;
	}
}

Outline::Outline(int side_count, int slope_count) : side_count(side_count), slope_count(slope_count) {
	int total = side_count + slope_count;

	this->center_foot.x = flnan;

	if (total > 0) {
		this->dots = new ProfileDot[total];
		this->sides = this->dots;
		this->slopes = &this->dots[side_count];
	}
}

Outline::Outline(const Outline* src) : Outline(src->side_count, src->slope_count) {
	this->clone_from(src);
}

void Outline::clone_from(const Outline* src) {
	int side_mcount = fxmin(this->side_count, src->side_count);
	int slope_mcount = fxmin(this->slope_count, src->slope_count);

	this->center_foot = src->center_foot;
	this->center_origin = src->center_origin;

	for (int idx = 0; idx < this->side_count; idx++) {
		if (idx < side_mcount) {
			this->sides[idx] = src->sides[idx];
		} else {
			this->sides[idx].x = flnan;
		}
	}

	for (int idx = 0; idx < this->slope_count; idx++) {
		if (idx < slope_mcount) {
			this->slopes[idx] = src->slopes[idx];
		} else {
			this->slopes[idx].x = flnan;
		}
	}

	std::sort(this->dots, &this->dots[this->side_count + this->slope_count], plane_dot_compare);
}
