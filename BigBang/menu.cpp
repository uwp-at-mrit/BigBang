#include "menu.hpp"
#include "planet.hpp"
#include "tongue.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

static IGraphlet* cpp_is_really_ugly_and_template_is_totally_bullshit = nullptr;

IGraphlet* WarGrey::SCADA::menu_current_target_graphlet() {
	return cpp_is_really_ugly_and_template_is_totally_bullshit;
}

void WarGrey::SCADA::menu_append_command(MenuFlyout^ menu_background, Platform::String^ label, ICommand^ exe, Platform::String^ tongue) {
	auto item = ref new MenuFlyoutItem();

	item->Text = speak(label, tongue);
	item->Command = exe;

	menu_background->Items->Append(item);
}

void WarGrey::SCADA::menu_popup(MenuFlyout^ m, IGraphlet* g, float local_x, float local_y, float xoff, float yoff) {
	IPlanet* dashboard = g->master();

	if (dashboard != nullptr) {
		Point position = dashboard->local_to_global_point(g, local_x, local_y, xoff, yoff);

		cpp_is_really_ugly_and_template_is_totally_bullshit = g;
		m->ShowAt(dashboard->master()->canvas, position);
	}
}
