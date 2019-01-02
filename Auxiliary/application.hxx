#pragma once

#include <ppltasks.h>

#include "syslog.hpp"
#include "system.hpp"
#include "backtask.hxx"

namespace WarGrey::SCADA {
	template<class UniversalWindowsScreen>
	private ref class UniversalWindowsApplication sealed : public Windows::UI::Xaml::Application {
	protected:
		void AppMain(Windows::UI::ViewManagement::ApplicationView^ self, UniversalWindowsScreen^ screen) {
			/** NOTE
			* the Titlebar in Universal Windows Platform is freaky,
			* it *can* be customized fully, but the caption buttons are always there.
			*
			* therefore, the default TitleBar is forced using here,
			* and instead, the FullScreen mode is preferred.
			*/

			Windows::ApplicationModel::Core::CoreApplication::GetCurrentView()->TitleBar->ExtendViewIntoTitleBar = false;
			
#if _DEBUG
			Windows::UI::ViewManagement::ApplicationView::PreferredLaunchWindowingMode = Windows::UI::ViewManagement::ApplicationViewWindowingMode::PreferredLaunchViewSize;
			Windows::UI::ViewManagement::ApplicationView::PreferredLaunchViewSize = system_screen_size();
#else
			Windows::UI::ViewManagement::ApplicationView::PreferredLaunchWindowingMode = Windows::UI::ViewManagement::ApplicationViewWindowingMode::FullScreen;
#endif

			Windows::ApplicationModel::Core::CoreApplication::UnhandledErrorDetected
				+= ref new Windows::Foundation::EventHandler<Windows::ApplicationModel::Core::UnhandledErrorDetectedEventArgs^>(this,
					&UniversalWindowsApplication::OnUncaughtException);

			this->EnteredBackground += ref new Windows::UI::Xaml::EnteredBackgroundEventHandler(this, &UniversalWindowsApplication::OnEnteredBackground);
			this->LeavingBackground += ref new Windows::UI::Xaml::LeavingBackgroundEventHandler(this, &UniversalWindowsApplication::OnLeavingBackground);
			this->Suspending += ref new Windows::UI::Xaml::SuspendingEventHandler(this, &UniversalWindowsApplication::OnSuspending);
			this->Resuming += ref new Windows::Foundation::EventHandler<Platform::Object^>(this, &UniversalWindowsApplication::OnResuming);
			this->RequestedTheme = Windows::UI::Xaml::ApplicationTheme::Dark;

			self->Title = screen->ToString();
		}

	protected:
		void OnLaunched(Windows::ApplicationModel::Activation::LaunchActivatedEventArgs^ e) override {
			Windows::UI::ViewManagement::ApplicationView^ self = Windows::UI::ViewManagement::ApplicationView::GetForCurrentView();
			UniversalWindowsScreen^ screen = dynamic_cast<UniversalWindowsScreen^>(Windows::UI::Xaml::Window::Current->Content);
			Platform::String^ package_name = Windows::ApplicationModel::Package::Current->DisplayName;
			
			if (screen == nullptr) {
				screen = ref new UniversalWindowsScreen();

				this->AppMain(self, screen);
				//this->BackgroundMain(package_name);

				if (e->PreviousExecutionState == Windows::ApplicationModel::Activation::ApplicationExecutionState::Terminated) {
					// TODO: Restore the saved session state only when appropriate, scheduling the
					// final launch steps after the restore is complete
				}

				Windows::UI::Xaml::Window::Current->Content = screen;
			}

			if (e->PrelaunchActivated == false) {
				screen->construct(package_name, adjusted_workspace_size(self->VisibleBounds, screen));
				Windows::UI::Xaml::Window::Current->Activate();
			}
		}

		void OnBackgroundActivated(Windows::ApplicationModel::Activation::BackgroundActivatedEventArgs^ args) {
			// For In-Process background task, this functionality can be treated just like `IBackgroundTask.Run()`
			UniversalWindowsScreen^ screen = dynamic_cast<UniversalWindowsScreen^>(Windows::UI::Xaml::Window::Current->Content);

			if (screen != nullptr) {
				Windows::UI::Xaml::Application::OnBackgroundActivated(args);

				screen->on_background_activated(args->TaskInstance);
			}
		}

	private:
		void OnEnteredBackground(Platform::Object^ sender, Windows::ApplicationModel::EnteredBackgroundEventArgs^ args) {
			// Do not assume that the application will be terminated or resumed with the contents of memory still intact.
			UniversalWindowsScreen^ screen = dynamic_cast<UniversalWindowsScreen^>(Windows::UI::Xaml::Window::Current->Content);

			if (screen != nullptr) {
				screen->on_entered_background(args);
			}
		}

		void OnSuspending(Platform::Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ args) {
			// This functionality should be replaced by EnterBackground;
			UniversalWindowsScreen^ screen = dynamic_cast<UniversalWindowsScreen^>(Windows::UI::Xaml::Window::Current->Content);

			if (screen != nullptr) {
				screen->on_suspending(args);
			}
		}

		void OnResuming(Platform::Object^ sender, Platform::Object^ args) {
			// This functionality should be replaced by LeavingBackground;
			UniversalWindowsScreen^ screen = dynamic_cast<UniversalWindowsScreen^>(Windows::UI::Xaml::Window::Current->Content);
		
			if (screen != nullptr) {
				screen->on_resuming();
			}
		}

		void OnLeavingBackground(Platform::Object^ sender, Windows::ApplicationModel::LeavingBackgroundEventArgs^ args) {
			// Only when any displayed content has changed while the app is suspended.
			UniversalWindowsScreen^ screen = dynamic_cast<UniversalWindowsScreen^>(Windows::UI::Xaml::Window::Current->Content);

			if (screen != nullptr) {
				screen->on_leaving_background(args);
			}
		}

		void OnUncaughtException(Platform::Object^ sender, Windows::ApplicationModel::Core::UnhandledErrorDetectedEventArgs^ args) {
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

	template<class UniversalWindowsScreen>
	int launch_universal_windows_application(WarGrey::SCADA::Log level, Platform::String^ remote_rsyslog_server, Platform::String^ lang = nullptr) {
		auto lazy_main = [](Windows::UI::Xaml::ApplicationInitializationCallbackParams^ p) {
			ref new WarGrey::SCADA::UniversalWindowsApplication<UniversalWindowsScreen>();
		};

#ifdef _DEBUG
		Windows::Globalization::ApplicationLanguages::PrimaryLanguageOverride
			= (lang == nullptr)
			? Windows::System::UserProfile::GlobalizationPreferences::Languages->GetAt(0)
			: lang;
#else
		Windows::Globalization::ApplicationLanguages::PrimaryLanguageOverride = "zh-CN";
#endif

		set_default_logging_level(level);
		set_default_racket_receiver_host(remote_rsyslog_server);


		Windows::UI::Xaml::Application::Start(ref new Windows::UI::Xaml::ApplicationInitializationCallback(lazy_main));

		return 0;
	}
}
