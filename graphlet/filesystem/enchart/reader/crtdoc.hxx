#pragma once

#include <deque>

#include "graphlet/filesystem/enchart/reader/enctype.hxx"

#include "datum/natural.hpp"

namespace WarGrey::DTPM {
	private ref class CertificateDoc sealed : public WarGrey::DTPM::ENChartDocument {
	internal:
		CertificateDoc(std::filebuf& dig);
	};
}
