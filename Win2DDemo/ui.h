#pragma once

#include "pch.h"

using namespace Windows::Foundation;
using namespace Windows::Globalization;
using namespace Windows::Globalization::DateTimeFormatting;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

typedef EventHandler<Object^> ObjectHandler;
typedef TypedEventHandler<CanvasControl^, CanvasDrawEventArgs^> CanvasDrawHandler;
typedef TypedEventHandler<CanvasControl^, CanvasCreateResourcesEventArgs^> CanvasRCHandler;

namespace Win2D {
    namespace XAML {
        public ref class DigitalClock sealed {
        public:
            DigitalClock(Panel^ parent);
            void ChangeSize(double width, double height);

        private:
            void UpdateTimeStamp();
            void OnTickUpdate(Object^ sender, Object^ e);
            void LoadTimestamp(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ args);
            void DrawClock(CanvasControl^ sender, CanvasDrawEventArgs^ args);

        private:
            String^ Timestamp = nullptr;
            CanvasControl^ entity = nullptr;

        private:
            Calendar^ datetime = nullptr;
            DispatcherTimer^ timer = nullptr;
            DateTimeFormatter^ formatter = nullptr;
        };

        public ref class UI sealed {
        public:
            static StackPanel^ MakeStackPanel(Panel^ parent, Orientation direction, Thickness margin, Thickness padding);
            static ToggleSwitch^ MakeToggleSwitch(Panel^ parent, String^ id, String^ onCaption, String^ offCaption);
            static CanvasControl^ MakeGPUCanvas(Panel^ parent, String^ id, CanvasRCHandler^ Load, CanvasDrawHandler^ Draw);
            static Canvas^ MakeCanvas(Panel^ parent, String^ id);

            static DispatcherTimer^ MakeGUITimer(long long ms, ObjectHandler^ handler);
        };
    }
}
