#include "graphlet/filesystem/project/reader/depthlog.hxx"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

/*************************************************************************************************/
DepthLog::DepthLog(std::filebuf& log) {
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

		this->depths.push_back(read_wgb18030(log, char_end_of_field));
		read_char(log);
		this->visibles.push_back(read_bool(log));
		discard_this_line(log);
	}
}
