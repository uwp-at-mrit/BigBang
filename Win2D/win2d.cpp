//
// pch.cpp
// Include the standard header and generate the precompiled header.
//

#include "pch.h"

int main(Platform::Array<Platform::String^>^ args) {
    auto UWPMain = [](ApplicationInitializationCallbackParams^ unused) { ref new Application(); };
    auto lazyApplication = ref new ApplicationInitializationCallback(UWPMain);
    Application::Start(lazyApplication);
}
