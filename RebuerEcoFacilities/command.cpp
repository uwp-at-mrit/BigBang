#include "command.hpp"
#include "tongue.hpp"
#include "syslog.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::UI::Xaml::Input;
using namespace Windows::UI::Xaml::Controls;

private ref class MenuCommand sealed : public ICommand {
internal:
	MenuCommand(CommandMenu* menu, IMenuCommand* exe, Menu cmd) : master(menu), executor(exe), command(cmd) {}

public:
	bool CanExecute(Platform::Object^ who_cares) override {
		return true;
	}

	void Execute(Platform::Object^ who_cares) override {
		this->executor->execute(this->command, this->master->current_snip());
	}

public:
	event EventHandler<Platform::Object^>^ CanExecuteChanged {
		// this event is useless in this project but to satisfy the C++/CX compiler
		EventRegistrationToken add(EventHandler<Platform::Object^>^ handler) override { return EventRegistrationToken{ 0L }; }
		void remove(EventRegistrationToken token) override {}
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

void CommandMenu::show(IPlanet* console, ISnip* snip, float x, float y) {
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
