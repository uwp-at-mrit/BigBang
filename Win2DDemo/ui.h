#pragma once

#include "pch.h"

using namespace Windows::Foundation;
using namespace Windows::Globalization;
using namespace Windows::Globalization::DateTimeFormatting;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

typedef void(*TimerEventHandler)(Object^, Object^);

namespace Win2D {
    namespace XAML {
        public ref class TextClock sealed : public TextBox {
        public:
            TextClock();

        private:
            long long UpdateTime();
            void OnTickAdjust(Object^ sender, Object^ e);
            void OnTickUpdate(Object^ sender, Object^ e);
            void OnLoadTrim(Object^ sender, RoutedEventArgs^ e);

        private:
            Calendar^ now = nullptr;
            DispatcherTimer^ timer = nullptr;
            DateTimeFormatter^ formatter = nullptr;
        };

        public ref class UI sealed {
        public:
            static StackPanel^ MakeStackPanel(Panel^ parent, Orientation direction, Thickness margin, Thickness padding);
            static ToggleSwitch^ MakeToggleSwitch(Panel^ parent, String^ id, String^ onCaption, String^ offCaption);
            static TextClock^ MakeTextClock(Panel^ parent);
            static Canvas^ MakeCanvas(Panel^ parent, String^ id);

            static DispatcherTimer^ MakeTimer(long long ms, EventHandler<Object^>^ handler);
        };
    }
}
