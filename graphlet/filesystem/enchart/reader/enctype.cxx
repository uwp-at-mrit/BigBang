#include "graphlet/filesystem/enchart/reader/enctype.hxx"
#include "graphlet/filesystem/enchart/reader/permitdoc.hxx"

#include "datum/string.hpp"
#include "tongue.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
Platform::String^ WarGrey::SCADA::enc_speak(ENCErrorCode ecode, Platform::String^ cell_name) {
	Platform::String^ strerrno = ecode.ToString();
	Platform::String^ leading = strerrno + ((cell_name == nullptr) ? ": " : "[" + cell_name + "]: ");

	return leading + speak(ecode.ToString(), "encerror");
}

Platform::String^ WarGrey::SCADA::enc_speak(WarGrey::SCADA::ENCErrorCode ecode, const char* cell_name) {
	return enc_speak(ecode, ((cell_name == nullptr) ? nullptr : make_wstring(cell_name)));
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
