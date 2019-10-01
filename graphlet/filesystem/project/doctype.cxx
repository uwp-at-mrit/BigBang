#include <ppltasks.h>

#include "graphlet/filesystem/project/doctype.hxx"
#include "graphlet/filesystem/project/digdoc.hxx"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;

/*************************************************************************************************/
IAsyncOperation<ProjectDocument^>^ ProjectDocument::load_async(Platform::String^ filename, WarGrey::SCADA::ProjectDoctype type) {
	return create_async([=] {
		ProjectDocument^ doc = nullptr;
		std::filebuf dig;

		if (dig.open(filename->Data(), std::ios::in)) {
			switch (type) {
			case ProjectDoctype::DIG_LOG: doc = ref new DigLog(dig); break;
			case ProjectDoctype::DIG: doc = ref new DigDoc(dig); break;
			}
		}

		return doc;
	});
}
