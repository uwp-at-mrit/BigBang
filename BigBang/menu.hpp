#pragma once

#include "object.hpp"

#include "forward.hpp"

namespace WarGrey::SCADA {
	WarGrey::SCADA::IGraphlet* menu_get_next_target_graphlet(WarGrey::SCADA::IGraphlet* start = nullptr);

	void menu_append_command(
		Windows::UI::Xaml::Controls::MenuFlyout^ master,
		Windows::UI::Xaml::Input::ICommand^ cmd,
		Platform::String^ label,
		Platform::String^ tongue = nullptr);

	void menu_popup(
		Windows::UI::Xaml::Controls::MenuFlyout^ master,
		WarGrey::SCADA::IGraphlet* g,
		float local_x, float local_y,
		float xoff = 0.0F, float yoff = 0.0F);

	void menu_popup(
		Windows::UI::Xaml::Controls::MenuFlyout^ master,
		WarGrey::SCADA::IPlanet* g,
		float x, float y,
		float xoff = 0.0F, float yoff = 0.0F);

	template<typename Menu, class Attachment>
	private class IMenuCommand abstract {
	public:
		virtual bool can_execute(Menu cmd, Attachment pobj) { return true; };

	public:
		virtual void begin_execute_sequence(Menu cmd, Attachment pobj) {}
		virtual void execute(Menu cmd, WarGrey::SCADA::IGraphlet* g, Attachment pobj) = 0;
		virtual void end_execute_sequence(Menu cmd, Attachment pobj) {}
	};

	template<typename Menu, class Attachment>
	private ref class MenuCommand sealed : public Windows::UI::Xaml::Input::ICommand {
		/** NOTE
		 * Interface linguistically is not class,
		 * all the required methods therefore should be marked as `virtual` instead of `override`.
		 */
	internal:
		MenuCommand(WarGrey::SCADA::IMenuCommand<Menu, Attachment>* exe, Menu cmd, Attachment pobj)
			: executor(exe), command(cmd), attachment(pobj) {}

	public:
		virtual bool CanExecute(Platform::Object^ parameter) {
			return this->executor->can_execute(this->command, this->attachment);
		}

		virtual void Execute(Platform::Object^ parameter) {
			IGraphlet* target = menu_get_next_target_graphlet(nullptr);

			this->executor->begin_execute_sequence(this->command, this->attachment);

			while (target != nullptr) {
				this->executor->execute(this->command, target, this->attachment);
				target = menu_get_next_target_graphlet(target);
			}

			this->executor->end_execute_sequence(this->command, this->attachment);
		}

	public:
		virtual event Windows::Foundation::EventHandler<Platform::Object^>^ CanExecuteChanged;

		void notify_status_change() {
			this->CanExecuteChanged(this, nullptr);
		}

	private:
		WarGrey::SCADA::IMenuCommand<Menu, Attachment>* executor;
		Menu command;
		Attachment attachment;
	};

	template<typename Menu, class Attachment>
	void menu_append_command(Windows::UI::Xaml::Controls::MenuFlyout^ m
		, WarGrey::SCADA::IMenuCommand<Menu, Attachment>* exe
		, Menu cmd, Attachment pobj, Platform::String^ tongue = nullptr) {
		WarGrey::SCADA::menu_append_command(m,
			ref new WarGrey::SCADA::MenuCommand<Menu, Attachment>(exe, cmd, pobj),
			cmd.ToString(), tongue);
	}

	template<typename Menu, class Attachment>
	void menu_append_command(Windows::UI::Xaml::Controls::MenuFlyout^ m
		, WarGrey::SCADA::IMenuCommand<Menu, Attachment>* exe
		, Menu start, Menu end, Attachment pobj, Platform::String^ tongue = nullptr) {
		for (Menu cmd = start; cmd <= end; cmd++) {
			menu_append_command(m, exe, cmd, pobj, tongue);
		}
	}

	template<typename Menu, class Attachment>
	Windows::UI::Xaml::Controls::MenuFlyout^ make_menu(WarGrey::SCADA::IMenuCommand<Menu, Attachment>* exe
		, Menu first, Menu last, Attachment pobj, Platform::String^ tongue = nullptr) {
		Windows::UI::Xaml::Controls::MenuFlyout^ m = ref new Windows::UI::Xaml::Controls::MenuFlyout();

		menu_append_command(m, exe, first, last, pobj, tongue);

		return m;
	}

	template<typename Menu, class Attachment>
	Windows::UI::Xaml::Controls::MenuFlyout^ make_menu(WarGrey::SCADA::IMenuCommand<Menu, Attachment>* exe
		, Attachment pobj, Platform::String^ tongue = nullptr) {
		Menu first_cmd = static_cast<Menu>(0);
		Menu last_cmd = static_cast<Menu>(static_cast<unsigned int>(Menu::_) - 1);

		return make_menu(exe, first_cmd, last_cmd, pobj, tongue);
	}
}
