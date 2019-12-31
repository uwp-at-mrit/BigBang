#include "graphlet/filesystem/project/reader/sectionlog.hxx"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

/*************************************************************************************************/
SectionLog::SectionLog(std::filebuf& log) {
	unsigned long long max_count = read_integer(log);
	
	discard_this_line(log);

	for (unsigned int c = 0; c < max_count; c++) {
		if (peek_char(log) == EOF) {
			break;
		}

		this->groups.push_back(read_integer(log));
		read_char(log);
		this->sections.push_back(read_wgb18030(log, char_end_of_field));
		discard_this_line(log);
	}
}
