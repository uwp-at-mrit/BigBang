#include "menu.hpp"
#include "planet.hpp"
#include "tongue.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

static ISnip* cpp_is_really_ugly_and_template_is_totally_a_bullshit = nullptr;

ISnip* WarGrey::SCADA::menu_current_target_snip() {
	return cpp_is_really_ugly_and_template_is_totally_a_bullshit;
}

void WarGrey::SCADA::menu_append(MenuFlyout^ menu, Platform::String^ label, ICommand^ exe) {
	auto item = ref new MenuFlyoutItem();

	item->Text = speak(label);
	item->Command = exe;

	menu->Items->Append(item);
}


void WarGrey::SCADA::menu_show(MenuFlyout^ menu, ISnip* snip, float local_x, float local_y, float xoff, float yoff) {
	IPlanet* console = snip->info->master;
	Point position = console->local_to_global_point(snip, local_x, local_y, xoff, yoff);

	cpp_is_really_ugly_and_template_is_totally_a_bullshit = snip;
	menu->ShowAt(console->info->master->canvas, position);
}
