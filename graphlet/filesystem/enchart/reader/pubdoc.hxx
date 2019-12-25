#pragma once

#include <deque>

#include "graphlet/filesystem/enchart/reader/enctype.hxx"

#include "datum/natural.hpp"

namespace WarGrey::SCADA {
	private ref class PublicKeyDoc sealed : public WarGrey::SCADA::ENChartDocument {
	internal:
		PublicKeyDoc(std::filebuf& dig);

	internal:
		WarGrey::SCADA::Natural p;
		WarGrey::SCADA::Natural q;
		WarGrey::SCADA::Natural g;
		WarGrey::SCADA::Natural y;
	};
}
