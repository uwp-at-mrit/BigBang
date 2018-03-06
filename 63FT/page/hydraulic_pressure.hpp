#pragma once

#include "planet.hpp"
#include "modbus.hpp"
#include "command.hpp"

#include "snip/togglet.hpp"
#include "snip/textlet.hpp"
#include "snip/statuslet.hpp"
#include "snip/pumplet.hpp"
#include "snip/gaugelet.hpp"
#include "snip/pipelinelet.hpp"

namespace WarGrey::SCADA {
	private class HPCWorkbench : public WarGrey::SCADA::Planet {
	public:
		~HPCWorkbench() noexcept;
		HPCWorkbench(Platform::String^ plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		void on_tap(ISnip* snip, float x, float y, bool shifted, bool ctrled) override;

	private:
		WarGrey::SCADA::IModbusClient* device;
		WarGrey::SCADA::IModbusConfirmation* console;
		WarGrey::SCADA::CommandMenu<WarGrey::SCADA::Menu>* cmdmenu;

	private: // never deletes these snips mannually	
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;
		WarGrey::SCADA::Togglet* shift;
	};
}
