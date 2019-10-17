#include "graphlet/filesystem/project/reader/maplog.hxx"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
MapLog::MapLog(std::filebuf& log) {
	unsigned long long max_count = read_integer(log);
	
	discard_this_line(log);

	for (unsigned int c = 0; c < max_count; c++) {
		if (peek_char(log) == EOF) {
			break;
		}

		this->digs.push_back(read_wgb18030(log, char_end_of_field));
		read_char(log);
		this->visibles.push_back(read_integer(log) == 1);
		discard_this_line(log);
	}
}
