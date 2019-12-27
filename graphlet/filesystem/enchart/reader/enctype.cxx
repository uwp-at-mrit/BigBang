#include "graphlet/filesystem/enchart/reader/enctype.hxx"

#include "graphlet/filesystem/enchart/reader/permitdoc.hxx"
#include "graphlet/filesystem/enchart/reader/pubdoc.hxx"
#include "graphlet/filesystem/enchart/reader/crtdoc.hxx"

#include "datum/string.hpp"
#include "tongue.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
Platform::String^ WarGrey::SCADA::enc_speak(ENCErrorCode ecode, Platform::String^ cell_name) {
	Platform::String^ strecode = ecode.ToString();
	Platform::String^ strerror = speak(strecode, "encerror");
	Platform::String^ message = "Stupid Microsoft"; // too many types to represent a string

	if (cell_name == nullptr) {
		message = make_wstring(L"%s: %s", strecode->Data(), strerror->Data());
	} else {
		message = make_wstring(L"%s[%s]: %s", strecode->Data(), cell_name->Data(), strerror->Data());
	}

	return message;
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
		case ENChartDoctype::PublicKey: doc = ref new PublicKeyDoc(src); break;
		case ENChartDoctype::Certificate: doc = ref new CertificateDoc(src); break;
		}
	}

	return doc;
}
