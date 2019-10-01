#include "graphlet/filesystem/project/xyzdoc.hxx"

#include "datum/flonum.hpp"
#include "datum/file.hpp"
#include "datum/box.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
XyzLog::XyzLog(std::filebuf& log) {
	unsigned long long max_count = read_integer(log);
	
	discard_this_line(log);

	this->min = read_flonum(log);
	read_char(log);
	this->max = read_flonum(log);
	discard_this_line(log);

	for (unsigned int c = 0; c < max_count; c++) {
		if (peek_char(log) == EOF) {
			break;
		}

		this->xyzs.push_back(read_wgb18030(log, char_end_of_field));
		read_char(log);
		this->visibles.push_back(read_integer(log) == 1);
		discard_this_line(log);
	}
}

/*************************************************************************************************/
XyzDoc::XyzDoc(std::filebuf& xyz) {
	while (peek_char(xyz) != EOF) {
		double3 depth;

		depth.y = read_flonum(xyz);
		discard_space(xyz);
		depth.x = read_flonum(xyz);
		discard_space(xyz);
		depth.z = read_flonum(xyz);
		discard_this_line(xyz);

		this->depths.push_back(depth);
	}
}
