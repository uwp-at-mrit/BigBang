#include "graphlet/filesystem/enchart/reader/crtdoc.hxx"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::DTPM;

/*************************************************************************************************/
CertificateDoc::CertificateDoc(std::filebuf& crt) {
	while (peek_char(crt) != EOF) {
		discard_this_line(crt);
	}
}
