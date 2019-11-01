#pragma once

#include <deque>

#include "graphlet/filesystem/project/reader/doctype.hxx"

namespace WarGrey::SCADA {
	private ref class SectionLog sealed : public WarGrey::SCADA::ProjectDocument {
	internal:
		SectionLog(std::filebuf& log);

	internal:
		std::deque<Platform::String^> sections;
		std::deque<long long> groups; // useless
	};
}
