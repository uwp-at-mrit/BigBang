#include "menu.hpp"
#include "planet.hpp"
#include "tongue.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

static IGraphlet* cpp_is_really_ugly_and_template_is_totally_a_bullshit = nullptr;

IGraphlet* WarGrey::SCADA::menu_current_target_graphlet() {
	return cpp_is_really_ugly_and_template_is_totally_a_bullshit;
}

void WarGrey::SCADA::menu_append(MenuFlyout^ menu_background, Platform::String^ label, ICommand^ exe) {
	auto item = ref new MenuFlyoutItem();

	item->Text = speak(label);
	item->Command = exe;

	menu_background->Items->Append(item);
}


void WarGrey::SCADA::menu_show(MenuFlyout^ menu_background, IGraphlet* g, float local_x, float local_y, float xoff, float yoff) {
	IPlanet* dashboard = g->info->master;
	Point position = dashboard->local_to_global_point(g, local_x, local_y, xoff, yoff);

	cpp_is_really_ugly_and_template_is_totally_a_bullshit = g;
	menu_background->ShowAt(dashboard->info->master->canvas, position);
}
