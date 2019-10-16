#pragma once

#include <deque>

#include "graphlet/filesystem/project/reader/doctype.hxx"

#include "graphlet/symbol/dig/dig.hpp"

#include "datum/flonum.hpp"

namespace WarGrey::SCADA {
	private ref class XyzLog sealed : public WarGrey::SCADA::ProjectDocument {
	internal:
		XyzLog(std::filebuf& dig);

	internal:
		double min;
		double max;
		std::deque<Platform::String^> xyzs;
		std::deque<bool> visibles;
	};

	private ref class XyzDoc sealed : public WarGrey::SCADA::ProjectDocument {
	internal:
		XyzDoc(std::filebuf& dig);

	internal:
		std::deque<WarGrey::SCADA::double3> depths;
	};
}
