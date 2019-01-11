#pragma once

#include "virtualization/keyboard.hpp"

namespace WarGrey::SCADA {
    private class Datepad : public WarGrey::SCADA::Keyboard {
    public:
		Datepad(WarGrey::SCADA::IPlanet* master, float fontsize = 32.0F);

	public:
		void fill_auto_position(float* x, float* y, WarGrey::SCADA::IGraphlet* g, WarGrey::SCADA::GraphletAnchor a) override;

	protected:
		Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ key_label(Windows::System::VirtualKey key) override;
	};
}
