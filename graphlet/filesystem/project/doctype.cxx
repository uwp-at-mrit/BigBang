#include <ppltasks.h>

#include "graphlet/filesystem/project/doctype.hxx"
#include "graphlet/filesystem/project/digdoc.hxx"
#include "graphlet/filesystem/project/xyzdoc.hxx"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;

/*************************************************************************************************/
IAsyncOperation<ProjectDocument^>^ ProjectDocument::load_async(Platform::String^ filename, WarGrey::SCADA::ProjectDoctype type) {
	return create_async([=] {
		ProjectDocument^ doc = nullptr;
		std::filebuf src;

		if (src.open(filename->Data(), std::ios::in)) {
			switch (type) {
			case ProjectDoctype::DIG_LOG: doc = ref new DigLog(src); break;
			case ProjectDoctype::DIG: doc = ref new DigDoc(src); break;
			case ProjectDoctype::XYZ_LOG: doc = ref new XyzLog(src); break;
			case ProjectDoctype::XYZ: doc = ref new XyzDoc(src); break;
			}
		}

		return doc;
	});
}
