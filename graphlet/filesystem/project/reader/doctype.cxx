#include <ppltasks.h>

#include "graphlet/filesystem/project/reader/doctype.hxx"
#include "graphlet/filesystem/project/reader/maplog.hxx"
#include "graphlet/filesystem/project/reader/depthlog.hxx"
#include "graphlet/filesystem/project/reader/sectionlog.hxx"
#include "graphlet/filesystem/project/reader/digdoc.hxx"
#include "graphlet/filesystem/project/reader/xyzdoc.hxx"
#include "graphlet/filesystem/project/reader/jobdoc.hxx"
#include "graphlet/filesystem/project/reader/secdoc.hxx"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;

/*************************************************************************************************/
ProjectDocument^ ProjectDocument::load(Platform::String^ filename, WarGrey::SCADA::ProjectDoctype type) {
	ProjectDocument^ doc = nullptr;
	std::filebuf src;

	if (src.open(filename->Data(), std::ios::in)) {
		switch (type) {
		case ProjectDoctype::DIG:         doc = ref new DigDoc(src); break;
		case ProjectDoctype::XYZ:         doc = ref new XyzDoc(src); break;
		case ProjectDoctype::SEC:         doc = ref new SecDoc(src); break;
		case ProjectDoctype::Traceline:   doc = ref new JobDoc(src); break;
		case ProjectDoctype::Map_LOG:     doc = ref new MapLog(src); break;
		case ProjectDoctype::Depth_LOG:   doc = ref new DepthLog(src); break;
		case ProjectDoctype::Section_LOG: doc = ref new SectionLog(src); break;
		}
	}

	return doc;
}
