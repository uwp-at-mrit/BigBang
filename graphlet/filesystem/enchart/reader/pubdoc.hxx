#pragma once

#include <deque>

#include "graphlet/filesystem/enchart/reader/enctype.hxx"

#include "datum/natural.hpp"

namespace WarGrey::DTPM {
	private ref class PublicKeyDoc sealed : public WarGrey::DTPM::ENChartDocument {
	internal:
		PublicKeyDoc(std::filebuf& dig);

	internal:
		WarGrey::SCADA::Natural p;
		WarGrey::SCADA::Natural q;
		WarGrey::SCADA::Natural g;
		WarGrey::SCADA::Natural y;
	};
}
