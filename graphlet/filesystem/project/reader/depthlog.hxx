#pragma once

#include <deque>

#include "graphlet/filesystem/project/reader/doctype.hxx"

namespace WarGrey::DTPM {
	private ref class DepthLog sealed : public WarGrey::DTPM::ProjectDocument {
	internal:
		DepthLog(std::filebuf& log);

	internal:
		double min;
		double max;
		std::deque<Platform::String^> depths;
		std::deque<bool> visibles;
	};
}
