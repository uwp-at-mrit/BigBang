#pragma once

#include "virtualization/keyboard.hpp"

namespace WarGrey::SCADA {
    private class Numpad : public WarGrey::SCADA::Keyboard {
    public:
		Numpad(WarGrey::SCADA::IPlanet* master, float fontsize = 32.0F);
		
	public:
		bool on_key(Windows::System::VirtualKey key, bool wargrey_keyboard) override;

	protected:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ key_label(Windows::System::VirtualKey key) override;
		Windows::System::VirtualKey find_received_key(unsigned int keycode) override;
	};
}
