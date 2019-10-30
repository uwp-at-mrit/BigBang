#pragma once

#include <deque>

#include "graphlet/filesystem/project/reader/doctype.hxx"

namespace WarGrey::SCADA {
	private ref class MapLog sealed : public WarGrey::SCADA::ProjectDocument {
	internal:
		MapLog(std::filebuf& log);

	internal:
		std::deque<Platform::String^> maps;
		std::deque<bool> visibles;
	};
}
