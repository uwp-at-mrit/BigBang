#include "command.hpp"
#include "tongue.hpp"

using namespace WarGrey::SCADA;

CommandMenu<Menu>* WarGrey::SCADA::make_start_stop_menu(IMenuCommand<Menu>* exe) {
	CommandMenu<Menu>* cmd = new CommandMenu<Menu>();

	cmd->reference();
	cmd->append(Menu::Start, exe);
	cmd->append(Menu::Stop, exe);

	return cmd;
}
