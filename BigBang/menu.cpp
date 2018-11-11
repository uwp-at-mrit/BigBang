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

/*************************************************************************************************/
void WarGrey::SCADA::menu_append_command(MenuFlyout^ menu_background, ICommand^ exe, Platform::String^ label, Platform::String^ tongue) {
	auto item = ref new MenuFlyoutItem();

	item->Command = exe;
	item->Text = speak(label, ((tongue == nullptr) ? "menu" : tongue));
	
	menu_background->Items->Append(item);
}

void WarGrey::SCADA::group_menu_append_command(MenuFlyout^ menu_background, ICommand^ exe
	, Platform::String^ group, Platform::String^ label, Platform::String^ maybe_tongue) {
	auto item = ref new MenuFlyoutItem();
	Platform::String^ tongue = ((maybe_tongue == nullptr) ? "menu" : maybe_tongue);

	item->Command = exe;
	item->Text = speak(group, tongue) + ": " + speak(label, tongue);

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

void WarGrey::SCADA::group_menu_popup(MenuFlyout^ m, IPlanet* p, float x, float y, float xoff, float yoff) {
	if (p != nullptr) {
		Point position(x, y);

		m->ShowAt(p->master()->canvas, position);
	}
}
