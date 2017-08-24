#pragma once

#include "ui.h"

using namespace Win2D::Xaml;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::ApplicationModel;

namespace Win2D {
    namespace StartsWithPanel {
        /// <summary> An empty page that can be used on its own or navigated to within a Frame. </summary>
        [::Windows::Foundation::Metadata::WebHostHidden]
        ref class WorkSpace sealed : public StackPanel {
        public:
            WorkSpace();

        public:
            void InitializeComponent();
            void Suspend(Object^ sender, SuspendingEventArgs^ e);

        private:
            void Reflow(Object^ sender, SizeChangedEventArgs^ e);

        private:
            StackPanel^ switchBar;
            DigitalClock^ systemClock;
            
            ToggleSwitch^ numeric;
            ToggleSwitch^ alert;
            ToggleSwitch^ flash;
            Canvas^ monitor;
        };
    }
}
