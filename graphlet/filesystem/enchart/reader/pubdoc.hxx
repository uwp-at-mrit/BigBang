#pragma once

#include <deque>

#include "graphlet/filesystem/enchart/reader/enctype.hxx"

#include "datum/natural.hpp"

namespace WarGrey::DTPM {
	private ref class PublicKeyDoc sealed : public WarGrey::DTPM::ENChartDocument {
	internal:
		PublicKeyDoc(std::filebuf& dig);

	internal:
		WarGrey::GYDM::Natural p;
		WarGrey::GYDM::Natural q;
		WarGrey::GYDM::Natural g;
		WarGrey::GYDM::Natural y;
	};
}
