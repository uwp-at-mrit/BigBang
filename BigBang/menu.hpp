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

	template<typename Menu, class Parameter>
	private class IMenuCommand abstract {
	public:
		virtual bool can_execute(Menu cmd, Parameter parameter) { return true; };
		virtual void execute(Menu cmd, WarGrey::SCADA::IGraphlet* g, Parameter parameter) = 0;
	};

	template<typename Menu, class Parameter>
	private ref class MenuCommand sealed : public Windows::UI::Xaml::Input::ICommand {
		/** NOTE	
		 * Interface linguistically is not class,
		 * all the required methods therefore should be marked as `virtual` instead of `override`.
		 */
	internal:
		MenuCommand(WarGrey::SCADA::IMenuCommand<Menu, Parameter>* exe, Menu cmd, Parameter p)
			: executor(exe), command(cmd), parameter(p) {}
		
	public:
		virtual bool CanExecute(Platform::Object^ parameter) {
			return this->executor->can_execute(this->command, this->parameter);
		}

		virtual void Execute(Platform::Object^ parameter) {
			IGraphlet* target = menu_get_next_target_graphlet(nullptr);

			while (target != nullptr) {
				this->executor->execute(this->command, target, this->parameter);
				target = menu_get_next_target_graphlet(target);
			}
		}

	public:
		virtual event Windows::Foundation::EventHandler<Platform::Object^>^ CanExecuteChanged;
		
		void notify_status_change() {
			this->CanExecuteChanged(this, nullptr);
		}

	private:
		WarGrey::SCADA::IMenuCommand<Menu, Parameter>* executor;
		Menu command;
		Parameter parameter;
	};

	template<typename Menu, class Parameter>
	void menu_append_command(Windows::UI::Xaml::Controls::MenuFlyout^ m, WarGrey::SCADA::IMenuCommand<Menu, Parameter>* exe
		, Menu cmd, Parameter p, Platform::String^ tongue = nullptr) {
		WarGrey::SCADA::menu_append_command(m,
			ref new WarGrey::SCADA::MenuCommand<Menu, Parameter>(exe, cmd, p),
			cmd.ToString(), tongue);
	}

	template<typename Menu, class Parameter>
	void menu_append_command(Windows::UI::Xaml::Controls::MenuFlyout^ m, WarGrey::SCADA::IMenuCommand<Menu, Parameter>* exe
		, Menu start, Menu end, Parameter p, Platform::String^ tongue = nullptr) {
		for (Menu cmd = start; cmd <= end; cmd++) {
			menu_append_command(m, exe, cmd, p, tongue);
		}
	}

	template<typename Menu, class Parameter>
	Windows::UI::Xaml::Controls::MenuFlyout^ make_menu(WarGrey::SCADA::IMenuCommand<Menu, Parameter>* exe, Parameter p, Platform::String^ tongue = nullptr) {
		Menu first_cmd = static_cast<Menu>(0);
		Menu last_cmd = static_cast<Menu>(static_cast<unsigned int>(Menu::_) - 1);
		Windows::UI::Xaml::Controls::MenuFlyout^ m = ref new Windows::UI::Xaml::Controls::MenuFlyout();

		menu_append_command(m, exe, first_cmd, last_cmd, p, tongue);

		return m;
	}
}
