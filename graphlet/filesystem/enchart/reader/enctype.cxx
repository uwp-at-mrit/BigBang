#include "graphlet/filesystem/enchart/reader/enctype.hxx"
#include "graphlet/filesystem/enchart/reader/permitdoc.hxx"

using namespace WarGrey::SCADA;

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
