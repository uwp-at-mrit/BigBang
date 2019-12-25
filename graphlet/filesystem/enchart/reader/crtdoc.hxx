#pragma once

#include <deque>

#include "graphlet/filesystem/enchart/reader/enctype.hxx"

#include "datum/natural.hpp"

namespace WarGrey::SCADA {
	private ref class CertificateDoc sealed : public WarGrey::SCADA::ENChartDocument {
	internal:
		CertificateDoc(std::filebuf& dig);
	};
}
