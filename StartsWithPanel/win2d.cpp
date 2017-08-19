#include "pch.h"
#include "workspace.h"

using namespace Windows::UI::Xaml;
using namespace Windows::ApplicationModel;
using namespace Windows::ApplicationModel::Activation;

namespace Win2D {
    namespace StartsWithPanel {
        /// <summary> Provides application-specific behavior to supplement the default Application class. </summary>
        ref class Win2D sealed : public Application {
        internal:
            /// <summary> Initializes the singleton application object, logical equal to main() or WinMain(). </summary>
            Win2D() {
                // InitializeComponent(); // [Debug] To catch unhandled exceptions that thrown by XAML parser.
                Suspending += ref new SuspendingEventHandler(this, &Win2D::OnSuspending);
            }

        protected:
            /// <summary> Invoked whenever the application is launched. </summary>
            /// <param name="e">Details about the launch request and process.</param>
            virtual void OnLaunched(LaunchActivatedEventArgs^ e) override {
                auto workspace = dynamic_cast<WorkSpace^>(Window::Current->Content);

                if (workspace != nullptr) {
                    if (e->PrelaunchActivated == false) {
                        workspace->InitializeComponent();
                        Window::Current->Activate();
                    }
                } else {
                    // Create a Frame to act as the navigation context and associate it with a SuspensionManager key
                    workspace = ref new WorkSpace();

                    if (e->PreviousExecutionState == ApplicationExecutionState::Terminated) {
                        // TODO: Restore the saved session state only when appropriate, scheduling the
                        // final launch steps after the restore is complete
                    }

                    if (e->PrelaunchActivated == false) {
                        workspace->InitializeComponent();
                        Window::Current->Content = workspace;
                        Window::Current->Activate();
                    }
                }
            }

        private:
            /// <summary> Invoked when application execution is being suspended. </summary>
            /// <param name="sender">The source of the suspend request.</param>
            /// <param name="e">Details about the suspend request.</param>
            void OnSuspending(Object^ sender, SuspendingEventArgs^ e) {
                // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
                auto workspace = dynamic_cast<WorkSpace^>(Window::Current->Content);
                if (workspace != nullptr) workspace->Suspend(sender, e);
            }
        };
    }
}

int main(Platform::Array<Platform::String^>^ args) {
    auto UWPMain = [](ApplicationInitializationCallbackParams^ p) { (void)p; ref new ::Win2D::StartsWithPanel::Win2D(); };
    auto lazyApplication = ref new ApplicationInitializationCallback(UWPMain);
    Application::Start(lazyApplication);
}
