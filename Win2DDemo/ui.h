#pragma once

#include "pch.h"

using namespace Windows::Foundation;
using namespace Windows::Globalization;
using namespace Windows::Globalization::DateTimeFormatting;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::UI::Xaml;

typedef EventHandler<Object^> ObjectHandler;
typedef TypedEventHandler<CanvasControl^, CanvasDrawEventArgs^> CanvasDrawHandler;
typedef TypedEventHandler<CanvasControl^, CanvasCreateResourcesEventArgs^> CanvasRCHandler;

namespace Win2D {
    namespace Xaml {
        public ref class Pasteboard : public DependencyObject {
        public:
            void ChangeSize(double width, double height);
            CanvasControl^ GetCanvas();

        internal:
            Pasteboard(Panel^ parent, String^ id);

            virtual void OnDisplaySize(double width, double height) {};
            virtual void LoadResources(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ args) {};
            virtual void Draw(CanvasControl^ sender, CanvasDrawingSession^ args) {};

        private:
            CanvasControl^ entity;

            void OnLoad(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ args);
            void OnPaint(CanvasControl^ sender, CanvasDrawEventArgs^ args);
        };

        public ref class DigitalClock sealed : public Pasteboard {
        public:
            DigitalClock(Panel^ parent);

        internal:
            void LoadResources(CanvasControl^ sender, CanvasCreateResourcesEventArgs^ args) override;
            void Draw(CanvasControl^ sender, CanvasDrawingSession^ args) override;

        private:
            void UpdateTimeStamp();
            void OnTickUpdate(Object^ sender, Object^ e);

        private:
            String^ timestamp;
            String^ datestamp;
            CanvasTextFormat^ fontInfo;

        private:
            Calendar^ datetime;
            DispatcherTimer^ timer;
            DateTimeFormatter^ longdate;
            DateTimeFormatter^ longtime;
        };

        public ref class XAML sealed {
        public:
            static StackPanel^ MakeStackPanel(Panel^ parent, Orientation direction, Thickness margin, Thickness padding);
            static ToggleSwitch^ MakeToggleSwitch(Panel^ parent, String^ id, String^ onCaption, String^ offCaption);
            static CanvasControl^ MakeGPUCanvas(Panel^ parent, String^ id, CanvasRCHandler^ Load, CanvasDrawHandler^ Draw);
            static Canvas^ MakeCanvas(Panel^ parent, String^ id);

            static DispatcherTimer^ MakeGUITimer(long long ms, ObjectHandler^ handler);
        };
    }
}
