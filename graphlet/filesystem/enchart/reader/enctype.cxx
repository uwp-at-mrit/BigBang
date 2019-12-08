#include "graphlet/filesystem/enchart/reader/enctype.hxx"
#include "graphlet/filesystem/enchart/reader/permitdoc.hxx"

#include "tongue.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
Platform::String^ WarGrey::SCADA::enc_speak(ENCErrorCode ecode) {
	Platform::String^ strerrno = ecode.ToString();

	return strerrno + ": " + speak(ecode.ToString(), "encerror");
}

/*************************************************************************************************/
ENChartDocument^ ENChartDocument::load(Platform::String^ filename, WarGrey::SCADA::ENChartDoctype type) {
	ENChartDocument^ doc = nullptr;
	std::filebuf src;

	if (src.open(filename->Data(), std::ios::in)) {
		switch (type) {
		case ENChartDoctype::PERMIT: doc = ref new PermitDoc(src); break;
		}
	}

	return doc;
}
