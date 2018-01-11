#pragma once

#include "universe.hpp"

namespace WarGrey::SCADA {
	private enum class RR { B1, Count };

    [::Windows::Foundation::Metadata::WebHostHidden]
	public ref class Console sealed : public Windows::UI::Xaml::Controls::SplitView {
	public:
		Console();
		void initialize_component(Windows::Foundation::Size region);

	public:
		void reflow(float width, float height);
		void suspend(Windows::ApplicationModel::SuspendingOperation^ op);

	private:
		~Console();

		void animating(Platform::Object^ sender, Windows::UI::Xaml::Input::ManipulationDeltaRoutedEventArgs^ e);
		void animated(Platform::Object^ sender, Windows::UI::Xaml::Input::ManipulationCompletedRoutedEventArgs^ e);

	private:
		Windows::UI::Xaml::Controls::StackPanel^ voids[static_cast<unsigned int>(RR::Count)];
		WarGrey::SCADA::IUniverse* universes[static_cast<unsigned int>(RR::Count)];
	};
}
