#pragma once

using namespace Platform;
using namespace Windows::ApplicationModel::Resources;

namespace Win2D {
    namespace UWRT {
        public ref class Tongue sealed {
        public:
            static String^ Speak(String^ word);
        };
    }
}
