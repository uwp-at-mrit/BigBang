#include "navigator/navigator.hpp"

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

void IUniverseNavigator::min_width(double v) {
	this->user_interface()->MinWidth = v;
}

float IUniverseNavigator::min_width() {
	return float(this->user_interface()->MinWidth);
}

void IUniverseNavigator::min_height(double v) {
	this->user_interface()->MinHeight = v;
}

float IUniverseNavigator::min_height() {
	return float(this->user_interface()->MinHeight);
}
