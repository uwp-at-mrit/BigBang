#include "tongue.hpp"

using namespace Windows::ApplicationModel::Resources;

Platform::String^ speak(Platform::String^ word) {
    static ResourceLoader^ tongue = ResourceLoader::GetForCurrentView("tongue");
    Platform::String^ dialect = tongue->GetString(word);

    return (dialect->Equals("")) ? word : dialect;
}