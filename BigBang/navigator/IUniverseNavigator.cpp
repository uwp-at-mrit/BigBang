#include "navigator/IUniverseNavigator.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;

/*************************************************************************************************/
void IUniverseNavigator::append_navigation_listener(IUniverseNavigatorListener^ listener) {
	this->listeners.push_back(listener);
}

void IUniverseNavigator::navigate(int from_index, int to_index) {
	for (IUniverseNavigatorListener^ listener : this->listeners) {
		listener->on_navigate(from_index, to_index);
	}
}
