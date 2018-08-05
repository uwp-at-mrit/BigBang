#pragma once

#include "object.hpp"

#include "forward.hpp"

namespace WarGrey::SCADA {
	WarGrey::SCADA::IGraphlet* menu_get_next_target_graphlet(WarGrey::SCADA::IGraphlet* start = nullptr);

	void menu_append_command(
		Windows::UI::Xaml::Controls::MenuFlyout^ master,
		Platform::String^ label,
		Windows::UI::Xaml::Input::ICommand^ cmd,
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

	template <typename Menu>
	private class IMenuCommand abstract {
	public:
		virtual void execute(Menu cmd, WarGrey::SCADA::IGraphlet* g) = 0;
	};

	template <typename Menu>
	private ref class MenuCommand sealed : public Windows::UI::Xaml::Input::ICommand {
		/** NOTE	
		 * Interface linguistically is not class,
		 * all the required methods therefore should be marked as `virtual` instead of `override`.
		 */
	internal:
		MenuCommand(WarGrey::SCADA::IMenuCommand<Menu>* exe, Menu cmd) : executor(exe), command(cmd) {}
		
	public:
		virtual bool CanExecute(Platform::Object^ who_cares) {
			return true;
		}

		virtual void Execute(Platform::Object^ who_cares) {
			WarGrey::SCADA::IGraphlet* target = menu_get_next_target_graphlet(nullptr);

			while (target != nullptr) {
				this->executor->execute(this->command, target);
				target = menu_get_next_target_graphlet(target);
			}
		}

	public:
		event Windows::Foundation::EventHandler<Platform::Object^>^ CanExecuteChanged {
			// this event is useless but to satisfy the C++/CX compiler
			virtual Windows::Foundation::EventRegistrationToken add(Windows::Foundation::EventHandler<Platform::Object^>^ handler) {
				return Windows::Foundation::EventRegistrationToken{ 0L };
			}

			virtual void remove(Windows::Foundation::EventRegistrationToken token) {}
		}

	private:
		WarGrey::SCADA::IMenuCommand<Menu>* executor;
		Menu command;
	};

	template<typename Menu>
	Windows::UI::Xaml::Controls::MenuFlyout^ make_menu(WarGrey::SCADA::IMenuCommand<Menu>* exe, Platform::String^ tongue = nullptr) {
		Windows::UI::Xaml::Controls::MenuFlyout^ m = ref new Windows::UI::Xaml::Controls::MenuFlyout();

		for (Menu cmd = static_cast<Menu>(0); cmd < Menu::_; cmd++) {
			menu_append_command(m, cmd.ToString(), ref new WarGrey::SCADA::MenuCommand<Menu>(exe, cmd));
		}

		return m;
	}
}
