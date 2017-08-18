#pragma once

#include "pch.h"

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

namespace Win2D {
    namespace XAML {
        public ref class UI sealed {
        public:
            static StackPanel^ MakeStackPanel(Panel^ parent, String^ id, Orientation direction);
            static ToggleSwitch^ MakeToggleSwitch(Panel^ parent, String^ id, String^ onCaption, String^ offCaption);
            static Canvas^ MakeCanvas(Panel^ parent, String^ id);
        };
    }
}
