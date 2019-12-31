#include "graphlet/filesystem/project/reader/secdoc.hxx"

#include "datum/file.hpp"
#include "datum/flonum.hpp"

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
