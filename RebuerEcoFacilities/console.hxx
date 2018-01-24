#pragma once

#include "planet.hpp"

namespace WarGrey::SCADA {
	private enum class RR { B1, B2, B3, B4, Count };

    [::Windows::Foundation::Metadata::WebHostHidden]
	public ref class Console sealed : public Windows::UI::Xaml::Controls::SplitView {
	public:
		Console();
		void initialize_component(Windows::Foundation::Size region);

	public:
		void suspend(Windows::ApplicationModel::SuspendingOperation^ op);

	private:
		void animate(Platform::Object^ sender, Windows::UI::Xaml::Input::ManipulationDeltaRoutedEventArgs^ e);
		void animating(Platform::Object^ sender, Windows::UI::Xaml::Input::ManipulationCompletedRoutedEventArgs^ e);
		void animated(Windows::UI::Xaml::Controls::SplitView^ sender, Platform::Object^ e);

		void do_paint(
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ sender,
			Microsoft::Graphics::Canvas::UI::Xaml::CanvasDrawEventArgs^ args);

	private:
		float transformX; 
		WarGrey::SCADA::UniverseDisplay^ universe;
		Windows::Foundation::TimeSpan duration;
		Microsoft::Graphics::Canvas::UI::Xaml::CanvasControl^ snapshot_panel;
		Windows::UI::Xaml::Controls::TextBlock^ labels[static_cast<unsigned int>(RR::Count)];
	};
}
