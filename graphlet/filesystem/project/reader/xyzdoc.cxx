#include "graphlet/filesystem/project/reader/xyzdoc.hxx"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;

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
