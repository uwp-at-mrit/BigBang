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

	template<typename Menu, class G, class Attachment>
	private class IMenuCommand abstract {
	public:
		virtual bool can_execute(Menu cmd, G* g, Attachment pobj, bool acc_executable) { return true; };
		
	public:
		virtual void begin_batch_sequence(Menu cmd, Attachment pobj) {}
		virtual void execute(Menu cmd, G* g, Attachment pobj) = 0;
		virtual void end_batch_sequence(Menu cmd, Attachment pobj) {}
	};

	template<typename Menu, class G, class Attachment>
	private ref class MenuCommand sealed : public Windows::UI::Xaml::Input::ICommand {
		/** NOTE
		 * Interfaces are not classes linguistically,
		 * all the required methods therefore should be marked as `virtual` instead of `override`.
		 */
	internal:
		MenuCommand(WarGrey::SCADA::IMenuCommand<Menu, G, Attachment>* exe, Menu cmd, Attachment pobj)
			: executor(exe), command(cmd), attachment(pobj) {}

	public:
		virtual bool CanExecute(Platform::Object^ parameter) {
			IGraphlet* maybe_target = menu_get_next_target_graphlet(nullptr);
			bool executable = true;

			while (maybe_target != nullptr) {
				auto target = dynamic_cast<G*>(maybe_target);

				if (target != nullptr) {
					executable = this->executor->can_execute(this->command, target, this->attachment, executable);
				}

				maybe_target = menu_get_next_target_graphlet(maybe_target);
			}

			return executable;
		}

		virtual void Execute(Platform::Object^ parameter) {
			IGraphlet* maybe_target = menu_get_next_target_graphlet(nullptr);

			this->executor->begin_batch_sequence(this->command, this->attachment);

			while (maybe_target != nullptr) {
				auto target = dynamic_cast<G*>(maybe_target);

				if ((target != nullptr) && (this->executor->can_execute(this->command, target, this->attachment, true))) {
					this->executor->execute(this->command, target, this->attachment);
				}

				maybe_target = menu_get_next_target_graphlet(maybe_target);
			}

			this->executor->end_batch_sequence(this->command, this->attachment);
		}

	public:
		virtual event Windows::Foundation::EventHandler<Platform::Object^>^ CanExecuteChanged;

		void notify_status_change() {
			this->CanExecuteChanged(this, nullptr);
		}

	private:
		WarGrey::SCADA::IMenuCommand<Menu, G, Attachment>* executor;
		Menu command;
		Attachment attachment;
	};

	template<typename Menu, class G, class Attachment>
	void menu_append_command(Windows::UI::Xaml::Controls::MenuFlyout^ m
		, WarGrey::SCADA::IMenuCommand<Menu, G, Attachment>* exe
		, Menu cmd, Attachment pobj, Platform::String^ tongue = nullptr) {
		WarGrey::SCADA::menu_append_command(m,
			ref new WarGrey::SCADA::MenuCommand<Menu, G, Attachment>(exe, cmd, pobj),
			cmd.ToString(), tongue);
	}

	template<typename Menu, class G, class Attachment>
	void menu_append_command(Windows::UI::Xaml::Controls::MenuFlyout^ m
		, WarGrey::SCADA::IMenuCommand<Menu, G, Attachment>* exe
		, Menu start, Menu end, Attachment pobj, Platform::String^ tongue = nullptr) {
		for (Menu cmd = start; cmd <= end; cmd++) {
			menu_append_command(m, exe, cmd, pobj, tongue);
		}
	}

	template<typename Menu, class G, class Attachment>
	Windows::UI::Xaml::Controls::MenuFlyout^ make_menu(WarGrey::SCADA::IMenuCommand<Menu, G, Attachment>* exe
		, Menu first, Menu last, Attachment pobj, Platform::String^ tongue = nullptr) {
		Windows::UI::Xaml::Controls::MenuFlyout^ m = ref new Windows::UI::Xaml::Controls::MenuFlyout();

		menu_append_command(m, exe, first, last, pobj, tongue);

		return m;
	}

	template<typename Menu, class G, class Attachment>
	Windows::UI::Xaml::Controls::MenuFlyout^ make_menu(WarGrey::SCADA::IMenuCommand<Menu, G, Attachment>* exe
		, Attachment pobj, Platform::String^ tongue = nullptr) {
		Menu first_cmd = static_cast<Menu>(0);
		Menu last_cmd = static_cast<Menu>(static_cast<unsigned int>(Menu::_) - 1);

		return WarGrey::SCADA::make_menu<Menu, G, Attachment>(exe, first_cmd, last_cmd, pobj, tongue);
	}
}
