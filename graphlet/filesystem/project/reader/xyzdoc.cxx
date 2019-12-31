#include "graphlet/filesystem/project/reader/xyzdoc.hxx"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

/*************************************************************************************************/
XyzDoc::XyzDoc(std::filebuf& xyz) {
	while (peek_char(xyz) != EOF) {
		double3 depth;

		depth.y = read_flonum(xyz);
		depth.x = read_flonum(xyz);
		depth.z = read_flonum(xyz);
		discard_this_line(xyz);

		this->depths.push_back(depth);
	}
}
