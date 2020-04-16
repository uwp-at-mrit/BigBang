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

void XyzDoc::append(WarGrey::DTPM::XyzDoc^ doc) {
	if (doc != nullptr) {
		for (auto it = doc->depths.begin(); it != doc->depths.end(); it ++) {
			this->depths.push_back(*it);
		}
	}
}
