#include "command.hpp"

using namespace WarGrey::SCADA;

CommandMenu<Menu>* WarGrey::SCADA::make_start_stop_menu(IMenuCommand<Menu>* exe) {
	CommandMenu<Menu>* cmd = new CommandMenu<Menu>();

	cmd->append(Menu::Start, exe);
	cmd->append(Menu::Stop, exe);

	return cmd;
}
