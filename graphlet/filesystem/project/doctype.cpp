#include <ppltasks.h>

#include "graphlet/filesystem/project/doctype.hxx"
#include "graphlet/filesystem/project/digdoc.hxx"

using namespace WarGrey::SCADA;

using namespace Concurrency;

/*************************************************************************************************/
IAsyncOperation<ProjectMap^>^ ProjectMap::load_async(Platform::String^ filename, WarGrey::SCADA::ProjectFileType type) {
	return create_async([=] {
		ProjectMap^ map = nullptr;
		std::filebuf dig;

		if (dig.open(filename->Data(), std::ios::in)) {
			map = ref new DigMap(dig);
		}

		return map;
	});
}
