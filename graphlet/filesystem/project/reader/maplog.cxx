#include "graphlet/filesystem/project/reader/maplog.hxx"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

/*************************************************************************************************/
MapLog::MapLog(std::filebuf& log) {
	unsigned long long max_count = read_integer(log);
	
	discard_this_line(log);

	for (unsigned int c = 0; c < max_count; c++) {
		if (peek_char(log) == EOF) {
			break;
		}

		this->maps.push_back(read_wgb18030(log, char_end_of_field));
		read_char(log);
		this->visibles.push_back(read_bool(log));
		discard_this_line(log);
	}
}
