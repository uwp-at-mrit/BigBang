#include "menu.hpp"
#include "planet.hpp"
#include "tongue.hpp"

#include "graphlet/primitive.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

static IPlanet* the_planet_for_multiple_selected_targets = nullptr;
static IGraphlet* the_specific_target = nullptr;

IGraphlet* WarGrey::SCADA::menu_get_next_target_graphlet(IGraphlet* start) {
	IGraphlet* target = nullptr;
	
	if (start == nullptr) {
		if (the_planet_for_multiple_selected_targets != nullptr) {
			target = the_planet_for_multiple_selected_targets->find_next_selected_graphlet(start);
		} else  {
			target = the_specific_target;
		}
	} else if (the_planet_for_multiple_selected_targets != nullptr) {
		target = the_planet_for_multiple_selected_targets->find_next_selected_graphlet(start);
	}
	
	return target;
}

void WarGrey::SCADA::menu_append_command(MenuFlyout^ menu_background, Platform::String^ label, ICommand^ exe, Platform::String^ tongue) {
	auto item = ref new MenuFlyoutItem();

	item->Text = speak(label, tongue);
	item->Command = exe;

	menu_background->Items->Append(item);
}

void WarGrey::SCADA::menu_popup(MenuFlyout^ m, IGraphlet* g, float local_x, float local_y, float xoff, float yoff) {
	IPlanet* p = g->master();

	if (p != nullptr) {
		Point position = p->local_to_global_point(g, local_x, local_y, xoff, yoff);

		the_planet_for_multiple_selected_targets = nullptr;
		the_specific_target = g;
		m->ShowAt(p->master()->canvas, position);
	}
}

void WarGrey::SCADA::menu_popup(MenuFlyout^ m, IPlanet* p, float x, float y, float xoff, float yoff) {
	if (p != nullptr) {
		Point position(x, y);

		the_planet_for_multiple_selected_targets = p;
		the_specific_target = nullptr;
		m->ShowAt(p->master()->canvas, position);
	}
}
