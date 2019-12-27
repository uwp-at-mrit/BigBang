#pragma once

// for concrete doctypes
#include <iostream>
#include <fstream>

#include "datum/bytes.hpp"

namespace WarGrey::SCADA {
	private enum class ENCErrorCode {
		SSE01, SSE02, SSE03, SSE04, SSE05, SSE06, SSE07, SSE08,
		SSE09, SSE10, SSE11, SSE12, SSE13, SSE14, SSE15, SSE16,
		SSE17, SSE18, SSE19, SSE20, SSE21, SSE22, SSE23, SSE24,
		SSE25, SSE26, SSE27,
		_
	};

	private enum class ENChartDoctype {
		PERMIT, PublicKey, Certificate,
		_
	};

	Platform::String^ enc_speak(WarGrey::SCADA::ENCErrorCode ecode, Platform::String^ cell_name = nullptr);
	Platform::String^ enc_speak(WarGrey::SCADA::ENCErrorCode ecode, const char* cell_name);

	private ref class ENChartDocument abstract {
	public:
		static WarGrey::SCADA::ENChartDocument^ load(Platform::String^ filename, WarGrey::SCADA::ENChartDoctype type);

	public:
		virtual ~ENChartDocument() {}
	};
}
