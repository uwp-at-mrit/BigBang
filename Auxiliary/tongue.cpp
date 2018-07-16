#include "tongue.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::ApplicationModel::Resources;

Platform::String^ WarGrey::SCADA::speak(Platform::String^ word) {
    static ResourceLoader^ tongue = ResourceLoader::GetForViewIndependentUse("tongue");
    Platform::String^ dialect = tongue->GetString(word);

    return (dialect == nullptr) ? word : dialect;
}

Platform::String^ WarGrey::SCADA::dbspeak(Platform::String^ word) {
	static ResourceLoader^ tongue = ResourceLoader::GetForViewIndependentUse("dbtongue");
	Platform::String^ dialect = tongue->GetString(word);

	return (dialect == nullptr) ? word : dialect;
}
