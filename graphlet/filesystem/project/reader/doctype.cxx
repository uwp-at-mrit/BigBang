#include <ppltasks.h>

#include "graphlet/filesystem/project/reader/doctype.hxx"
#include "graphlet/filesystem/project/reader/digdoc.hxx"
#include "graphlet/filesystem/project/reader/xyzdoc.hxx"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;

/*************************************************************************************************/
ProjectDocument^ ProjectDocument::load(Platform::String^ filename, WarGrey::SCADA::ProjectDoctype type) {
	ProjectDocument^ doc = nullptr;
	std::filebuf src;

	if (src.open(filename->Data(), std::ios::in)) {
		switch (type) {
		case ProjectDoctype::DIG_LOG: doc = ref new DigLog(src); break;
		case ProjectDoctype::DIG:     doc = ref new DigDoc(src); break;
		case ProjectDoctype::XYZ_LOG: doc = ref new XyzLog(src); break;
		case ProjectDoctype::XYZ:     doc = ref new XyzDoc(src); break;
		}
	}

	return doc;
}
