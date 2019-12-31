#pragma once

#include <deque>

#include "graphlet/filesystem/project/reader/doctype.hxx"

#include "datum/flonum.hpp"

namespace WarGrey::DTPM {
	private ref class XyzDoc sealed : public WarGrey::DTPM::ProjectDocument {
	internal:
		XyzDoc(std::filebuf& dig);

	internal:
		std::deque<WarGrey::SCADA::double3> depths;
	};
}
