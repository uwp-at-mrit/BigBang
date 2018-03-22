#include "cosmos.hxx"
#include "configuration.hpp"

#include "syslog.hpp"
#include "system.hpp"

using namespace Windows::Foundation;

using namespace Windows::ApplicationModel;
using namespace Windows::ApplicationModel::Activation;
using namespace Windows::ApplicationModel::Core;

using namespace Windows::UI::Xaml;
using namespace Windows::UI::Xaml::Controls;
using namespace Windows::UI::ViewManagement;

using namespace Windows::Globalization;
using namespace Windows::System::UserProfile;

namespace WarGrey::SCADA {
    typedef EventHandler<UnhandledErrorDetectedEventArgs^> UncaughtExceptionHandler;

    private ref class CH6000M3 sealed : public Application {
    protected:
        void AppMain(ApplicationView^ self, FrameworkElement^ screen) {
			ApplicationLanguages::PrimaryLanguageOverride = GlobalizationPreferences::Languages->GetAt(0);
			ApplicationView::PreferredLaunchWindowingMode = ApplicationViewWindowingMode::PreferredLaunchViewSize;
			ApplicationView::PreferredLaunchViewSize = system_screen_size();

			CoreApplication::GetCurrentView()->TitleBar->ExtendViewIntoTitleBar = false; // Force Using the default TitleBar.
			CoreApplication::UnhandledErrorDetected += ref new UncaughtExceptionHandler(this, &CH6000M3::OnUncaughtException);
			
			this->Suspending += ref new SuspendingEventHandler(this, &CH6000M3::OnSuspending);
            this->RequestedTheme = ApplicationTheme::Dark;

            self->Title = screen->ToString();

			set_default_racket_receiver_host(remote_test_server);
        }

        void OnLaunched(LaunchActivatedEventArgs^ e) override {
            auto self = ApplicationView::GetForCurrentView();
            auto screen = dynamic_cast<Cosmos^>(Window::Current->Content);

            if (screen == nullptr) {
                screen = ref new Cosmos();
                this->AppMain(self, screen);
            
                if (e->PreviousExecutionState == ApplicationExecutionState::Terminated) {
                    // TODO: Restore the saved session state only when appropriate, scheduling the
                    // final launch steps after the restore is complete
                }

				Window::Current->Content = screen;
            }

			if (e->PrelaunchActivated == false) {
				screen->initialize_component(adjusted_workspace_size(self->VisibleBounds, screen));
				Window::Current->Activate();
			}
        }

    private:
        void OnSuspending(Platform::Object^ sender, SuspendingEventArgs^ args) {
			// TODO: Save application state and stop any background activity.
            // Do not assume that the application will be terminated or resumed with the contents of memory still intact.
        }

        void OnUncaughtException(Platform::Object^ sender, UnhandledErrorDetectedEventArgs^ args) {
            auto error = args->UnhandledError;

            if (!error->Handled) {
                try {
                    // if an error is returned from a delegate, it will not be marked as Handled.
                    error->Propagate();
                } catch (Platform::Exception^ e) {
                    syslog(Log::Panic, "Unhandled Error: " + e->Message);
				}
            }
        }
    };
}

int main(Platform::Array<Platform::String^>^ args) {
	auto lazy_main = [](ApplicationInitializationCallbackParams^ p) { ref new WarGrey::SCADA::CH6000M3(); };
    Application::Start(ref new ApplicationInitializationCallback(lazy_main));
}
