﻿#pragma once

#include "syslog.hpp"
#include "system.hpp"

namespace WarGrey::SCADA {
	template<class UniversalWindowsScreen>
	private ref class UniversalWindowsApplication sealed : public Windows::UI::Xaml::Application {
	protected:
		void AppMain(Windows::UI::ViewManagement::ApplicationView^ self, Windows::UI::Xaml::FrameworkElement^ screen) {
#ifdef _DEBUG
			Windows::Globalization::ApplicationLanguages::PrimaryLanguageOverride = Windows::System::UserProfile::GlobalizationPreferences::Languages->GetAt(0);
#else
			Windows::Globalization::ApplicationLanguages::PrimaryLanguageOverride = "zh-CN";
#endif
			Windows::UI::ViewManagement::ApplicationView::PreferredLaunchWindowingMode = Windows::UI::ViewManagement::ApplicationViewWindowingMode::PreferredLaunchViewSize;
			Windows::UI::ViewManagement::ApplicationView::PreferredLaunchViewSize = system_screen_size();

			Windows::ApplicationModel::Core::CoreApplication::GetCurrentView()->TitleBar->ExtendViewIntoTitleBar = false; // Force Using the default TitleBar.
			Windows::ApplicationModel::Core::CoreApplication::UnhandledErrorDetected
				+= ref new Windows::Foundation::EventHandler<Windows::ApplicationModel::Core::UnhandledErrorDetectedEventArgs^>(this,
					&UniversalWindowsApplication::OnUncaughtException);

			this->Suspending += ref new Windows::UI::Xaml::SuspendingEventHandler(this, &UniversalWindowsApplication::OnSuspending);
			this->RequestedTheme = Windows::UI::Xaml::ApplicationTheme::Dark;

			self->Title = screen->ToString();
		}

		void OnLaunched(Windows::ApplicationModel::Activation::LaunchActivatedEventArgs^ e) override {
			Windows::UI::ViewManagement::ApplicationView^ self = Windows::UI::ViewManagement::ApplicationView::GetForCurrentView();
			UniversalWindowsScreen^ screen = dynamic_cast<UniversalWindowsScreen^>(Windows::UI::Xaml::Window::Current->Content);

			if (screen == nullptr) {
				screen = ref new UniversalWindowsScreen();
				this->AppMain(self, screen);

				if (e->PreviousExecutionState == Windows::ApplicationModel::Activation::ApplicationExecutionState::Terminated) {
					// TODO: Restore the saved session state only when appropriate, scheduling the
					// final launch steps after the restore is complete
				}

				Windows::UI::Xaml::Window::Current->Content = screen;
			}

			if (e->PrelaunchActivated == false) {
				screen->initialize_component(adjusted_workspace_size(self->VisibleBounds, screen));
				Windows::UI::Xaml::Window::Current->Activate();
			}
		}

	private:
		void OnSuspending(Platform::Object^ sender, Windows::ApplicationModel::SuspendingEventArgs^ args) {
			// TODO: Save application state and stop any background activity.
			// Do not assume that the application will be terminated or resumed with the contents of memory still intact.
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
	void launch_universal_windows_application(Platform::String^ remote_rsyslog_server = nullptr) {
		auto lazy_main = [](Windows::UI::Xaml::ApplicationInitializationCallbackParams^ p) {
			ref new WarGrey::SCADA::UniversalWindowsApplication<UniversalWindowsScreen>();
		};

		set_default_racket_receiver_host(remote_rsyslog_server);
		Windows::UI::Xaml::Application::Start(ref new Windows::UI::Xaml::ApplicationInitializationCallback(lazy_main));
	}
}