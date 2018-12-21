#include "navigator/null.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;

static UserControl^ null_control = nullptr;

/*************************************************************************************************/
void NullNavigator::insert(IPlanet* planet) {}

void NullNavigator::select(IPlanet* planet) {}

int NullNavigator::selected_index() {
	return -1;
}

Control^ NullNavigator::user_interface() {
	// TODO: Maybe it should just return a `nullptr`

	if (null_control == nullptr) {
		null_control = ref new UserControl();
	}

	return null_control;
}
