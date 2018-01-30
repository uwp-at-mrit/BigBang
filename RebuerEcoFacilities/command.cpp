#include "command.hpp"
#include "tongue.hpp"
#include "syslog.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

/** NOTE
 * interface linguistically is not a class
 * all the required methods therefore should be marked as `virtual` instead of `override`.
 */

private ref class MenuCommand sealed : public ICommand {
internal:
	MenuCommand(CommandMenu* menu, IMenuCommand* exe, Menu cmd) : master(menu), executor(exe), command(cmd) {}

public:
	virtual bool CanExecute(Platform::Object^ who_cares) {
		return true;
	}

	virtual void Execute(Platform::Object^ who_cares) {
		this->executor->execute(this->command, this->master->current_snip());
	}

public:
	event EventHandler<Platform::Object^>^ CanExecuteChanged {
		// this event is useless in this project but to satisfy the C++/CX compiler
		virtual EventRegistrationToken add(EventHandler<Platform::Object^>^ handler) { return EventRegistrationToken{ 0L }; }
		virtual void remove(EventRegistrationToken token) {}
	}

private:
	CommandMenu* master;
	IMenuCommand* executor;
	Menu command;
};

/*************************************************************************************************/
CommandMenu::CommandMenu() {
	this->menu = ref new MenuFlyout();
}

void CommandMenu::append(Menu cmd, IMenuCommand* exe) {
	auto item = ref new MenuFlyoutItem();

	item->Text = speak(cmd.ToString());
	item->Command = ref new MenuCommand(this, exe, cmd);

	menu->Items->Append(item);
}

void CommandMenu::show_for(ISnip* snip, float x, float y) {
	IPlanet* console = snip->info->master;

	this->snip = snip;
	this->menu->ShowAt(console->info->master->canvas, console->local_to_global_point(snip, x, y));
}

ISnip* CommandMenu::current_snip() {
	return this->snip;
}

/*************************************************************************************************/
CommandMenu* WarGrey::SCADA::make_start_stop_menu(IMenuCommand* exe) {
	CommandMenu* cmd = new CommandMenu();

	cmd->reference();
	cmd->append(Menu::Start, exe);
	cmd->append(Menu::Stop, exe);

	return cmd;
}
