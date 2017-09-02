#include "monitor.hxx"

namespace WarGrey::Win2DDemo {
    /// <summary> Provides application-specific behavior to supplement the default Application class. </summary>
    private ref class Application sealed : public Windows::UI::Xaml::Application {
    internal:
        /// <summary> Initializes the singleton application object, logical equal to main() or WinMain(). </summary>
        Application() {
            Suspending += ref new Windows::UI::Xaml::SuspendingEventHandler(this, &Application::OnSuspending);
        }

    protected:
        /// <summary> Invoked whenever the application is launched. </summary>
        /// <param name="e">Details about the launch request and process.</param>
        virtual void OnLaunched(Windows::ApplicationModel::Activation::LaunchActivatedEventArgs^ e) override {
            auto workspace = dynamic_cast<Monitor^>(Windows::UI::Xaml::Window::Current->Content);

            if (workspace != nullptr) {
                if (e->PrelaunchActivated == false) {
                    workspace->initialize_component();
                    Windows::UI::Xaml::Window::Current->Activate();
                }
            } else {
                // Create a Frame to act as the navigation context and associate it with a SuspensionManager key
                workspace = ref new Monitor();

                if (e->PreviousExecutionState == Windows::ApplicationModel::Activation::ApplicationExecutionState::Terminated) {
                    // TODO: Restore the saved session state only when appropriate, scheduling the
                        // final launch steps after the restore is complete
                }

                if (e->PrelaunchActivated == false) {
                    workspace->initialize_component();
                    Windows::UI::Xaml::Window::Current->Content = workspace;
                    Windows::UI::Xaml::Window::Current->Activate();
                }
            }
        }

    private:
        /// <summary> Invoked when application execution is being suspended. </summary>
        /// <param name="sender">The source of the suspend request.</param>
        /// <param name="e">Details about the suspend request.</param>
        void OnSuspending(Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ e) {
            // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
            auto workspace = dynamic_cast<Monitor^>(Windows::UI::Xaml::Window::Current->Content);
            if (workspace != nullptr) workspace->suspend(sender, e);
        }
    };
}

int main(Platform::Array<Platform::String^>^ args) {
    Windows::UI::Xaml::Application::Start(
        ref new Windows::UI::Xaml::ApplicationInitializationCallback(
            [](Windows::UI::Xaml::ApplicationInitializationCallbackParams^ p) {
        ref new WarGrey::Win2DDemo::Application(); }));
}
