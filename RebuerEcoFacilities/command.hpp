#pragma once

#include "object.hpp"
#include "planet.hpp"

namespace WarGrey::SCADA {
	private enum class Menu { Start, Stop };

	private class IMenuCommand {
	public:
		virtual void execute(Menu cmd, WarGrey::SCADA::ISnip* snip) = 0;
	};

	private class CommandMenu : public WarGrey::SCADA::SharedObject {
	public:
		CommandMenu();

	public:
		void append(WarGrey::SCADA::Menu cmd, WarGrey::SCADA::IMenuCommand* exe);
		void show(WarGrey::SCADA::IPlanet* planet, WarGrey::SCADA::ISnip* snip, float local_x, float local_y);
		WarGrey::SCADA::ISnip* current_snip();

	private:
		Windows::UI::Xaml::Controls::MenuFlyout^ menu;
		WarGrey::SCADA::IPlanet* planet;
		WarGrey::SCADA::ISnip* snip;
	};

	WarGrey::SCADA::CommandMenu* make_start_stop_menu(WarGrey::SCADA::IMenuCommand* exe);
}
