#pragma once

#include "planet.hpp"
#include "modbus.hpp"

#include "snip/textlet.hpp"
#include "snip/statuslet.hpp"
#include "snip/iconlet.hpp"
#include "snip/shapelet.hpp"

namespace WarGrey::SCADA {
	private class HPSingle : public WarGrey::SCADA::Planet {
	public:
		~HPSingle() noexcept;
		HPSingle(Platform::String^ plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		void on_tap(ISnip* snip, float x, float y, bool shifted, bool ctrled) override;

	private:
		WarGrey::SCADA::IModbusClient* device;
		WarGrey::SCADA::IModbusConfirmation* console;

	private: // never deletes these snips mannually	
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;

	private:
		float gridsize;
	};
}
