#pragma once

#include <deque>

#include "graphlet/filesystem/enchart/reader/enctype.hxx"

#include "datum/natural.hpp"

namespace WarGrey::DTPM {
	private ref class PublicKeyDoc sealed : public WarGrey::DTPM::ENChartDocument {
	internal:
		PublicKeyDoc(std::filebuf& dig);

	internal:
		WarGrey::DTPM::Natural p;
		WarGrey::DTPM::Natural q;
		WarGrey::DTPM::Natural g;
		WarGrey::DTPM::Natural y;
	};
}
