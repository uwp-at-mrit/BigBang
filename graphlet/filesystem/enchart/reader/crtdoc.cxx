#include "graphlet/filesystem/enchart/reader/crtdoc.hxx"

#include "datum/file.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
CertificateDoc::CertificateDoc(std::filebuf& crt) {
	Natural n;

	while (peek_char(crt) != EOF) {
		discard_this_line(crt);
	}
}
