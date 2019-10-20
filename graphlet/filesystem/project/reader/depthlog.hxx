#pragma once

#include <deque>

#include "graphlet/filesystem/project/reader/doctype.hxx"

#include "graphlet/symbol/dig/dig.hpp"

namespace WarGrey::SCADA {
	private ref class DepthLog sealed : public WarGrey::SCADA::ProjectDocument {
	internal:
		DepthLog(std::filebuf& dig);

	internal:
		double min;
		double max;
		std::deque<Platform::String^> depths;
		std::deque<bool> visibles;
	};
}