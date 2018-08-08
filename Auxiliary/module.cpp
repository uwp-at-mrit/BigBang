#include <map>

#include "module.hpp"
#include "path.hpp"

using namespace WarGrey::SCADA;

Platform::String^ WarGrey::SCADA::module_name(Platform::String^ src) {
	static std::map<Platform::String^, Platform::String^> modules;
	auto maybe_module = modules.find(src);

	if (maybe_module == modules.end()) {
		modules.insert(std::pair<Platform::String^, Platform::String^>(src, file_basename_from_path(src)));
		maybe_module = modules.find(src);
	}

	return maybe_module->second;
}
