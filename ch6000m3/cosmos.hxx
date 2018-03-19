#pragma once

#include "planet.hpp"

namespace WarGrey::SCADA {
    [::Windows::Foundation::Metadata::WebHostHidden]
	public ref class Cosmos sealed : public Windows::UI::Xaml::Controls::SplitView {
	public:
		Cosmos();

	public:
		void initialize_component(Windows::Foundation::Size region);
		
	private:
		void on_pointer_moved(Platform::Object^ sender, Windows::UI::Xaml::Input::PointerRoutedEventArgs^ args);
		
	private:
		WarGrey::SCADA::UniverseDisplay^ universe;
	};
}
