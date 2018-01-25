#pragma once

#include "planet.hpp"

namespace WarGrey::SCADA {
    [::Windows::Foundation::Metadata::WebHostHidden]
	public ref class Console sealed : public Windows::UI::Xaml::Controls::SplitView {
	public:
		Console();

	public:
		void initialize_component(Windows::Foundation::Size region);
		void suspend(Windows::ApplicationModel::SuspendingOperation^ op);

	private:
		void transfer(Platform::Object^ sender, Windows::UI::Xaml::Input::ManipulationCompletedRoutedEventArgs^ e);
		
	private:
		WarGrey::SCADA::UniverseDisplay^ universe;
	};
}
