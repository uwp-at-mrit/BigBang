#pragma once

// for concrete doctypes
#include <iostream>
#include <fstream>

namespace WarGrey::SCADA {
	private enum class ENChartDoctype {
		PERMIT,
		_
	};

	private ref class ENChartDocument abstract {
	public:
		static WarGrey::SCADA::ENChartDocument^ load(Platform::String^ filename, WarGrey::SCADA::ENChartDoctype type);

	public:
		virtual ~ENChartDocument() {}
	};
}
