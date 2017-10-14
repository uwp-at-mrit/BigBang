#include "tongue.hpp"

using namespace Windows::ApplicationModel::Resources;

Platform::String^ speak(Platform::String^ word) {
    static ResourceLoader^ tongue = ResourceLoader::GetForViewIndependentUse("tongue");
    Platform::String^ dialect = tongue->GetString(word);

    return (dialect->IsEmpty()) ? word : dialect;
}