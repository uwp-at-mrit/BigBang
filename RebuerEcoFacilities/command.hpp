#pragma once

#include "menu.hpp"

namespace WarGrey::SCADA {
	private enum class Menu { Start, Stop };

	WarGrey::SCADA::CommandMenu<Menu>* make_start_stop_menu(WarGrey::SCADA::IMenuCommand<Menu>* exe);
}
