#pragma once

#include <deque>

#include "graphlet/filesystem/project/reader/doctype.hxx"

#include "datum/flonum.hpp"

namespace WarGrey::SCADA {
	private ref class XyzDoc sealed : public WarGrey::SCADA::ProjectDocument {
	internal:
		XyzDoc(std::filebuf& dig);

	internal:
		std::deque<WarGrey::SCADA::double3> depths;
	};
}
