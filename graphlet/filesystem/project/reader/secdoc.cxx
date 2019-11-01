#include "graphlet/filesystem/project/reader/secdoc.hxx"

#include "datum/file.hpp"
#include "datum/flonum.hpp"

#include "math.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
SectionDot::SectionDot(double x, double y, double depth)
	: SectionDot(x, y, depth, 0.0, 0.0) {}

SectionDot::SectionDot(double x, double y, double depth, double slope_depth, double grade)
	: x(x), y(y), depth(depth), slope_depth(slope_depth), grade(grade) {}

/*************************************************************************************************/
SecDoc::SecDoc(std::filebuf& sec) {
	double x, y, depth, slope_depth, grade;
	
	{ // read centerline dots
		long long dot_num = read_integer(sec);

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

				this->centerline.push_back(SectionDot(x, y, depth));

				discard_this_line(sec);
			}
		}
	}

	{ // read side lines
		long long sideline_num = read_integer(sec);

		discard_this_line(sec);
		for (long long sidx = 0; sidx < sideline_num; sidx++) {
			std::deque<SectionDot> this_sideline;
			long long dot_num = read_integer(sec);
			
			discard_this_line(sec);
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

				this_sideline.push_back(SectionDot(x, y, depth, slope_depth, grade));

				discard_this_line(sec);
			}

			this->sidelines.push_back(this_sideline);
		}
	}
}
