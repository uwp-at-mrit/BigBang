#pragma once

using namespace Windows::UI::Xaml::Controls;
using namespace Windows::ApplicationModel;

namespace Win2D {
    /// <summary> An empty page that can be used on its own or navigated to within a Frame. </summary>
    [::Windows::Foundation::Metadata::WebHostHidden]
    ref class WorkSpace sealed : public StackPanel {
    public:
        WorkSpace();
        void InitializeComponent();
        void Suspend(Object^ sender, SuspendingEventArgs^ e);
    };
}
