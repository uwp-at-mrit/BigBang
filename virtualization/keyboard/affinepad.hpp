#pragma once

#include "virtualization/keyboard.hpp"

namespace WarGrey::SCADA {
    private class Affinepad : public WarGrey::SCADA::Keyboard {
    public:
		Affinepad(WarGrey::SCADA::IPlanet* master, float fontsize = 32.0F);

	public:
		void fill_auto_position(float* x, float* y, WarGrey::SCADA::IGraphlet* g, WarGrey::SCADA::GraphletAnchor a) override;

	protected:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ key_label(Windows::System::VirtualKey key) override;
		Windows::System::VirtualKey find_received_key(unsigned int keycode) override;
	};
}
