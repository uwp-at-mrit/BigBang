#pragma once

#include "planet.hpp"

namespace WarGrey::SCADA {
    [::Windows::Foundation::Metadata::WebHostHidden]
	public ref class Workbench sealed : public Windows::UI::Xaml::Controls::SplitView {
	public:
		Workbench();

	public:
		void initialize_component(Windows::Foundation::Size region);
		void suspend(Windows::ApplicationModel::SuspendingOperation^ op);
		
	private:
		WarGrey::SCADA::UniverseDisplay^ universe;
	};
}
