#pragma once

#include "planet.hpp"
#include "modbus.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"
#include "graphlet/shapelet.hpp"

#include "graphlet/pumplet.hpp"
#include "graphlet/valvelet.hpp"

namespace WarGrey::SCADA {
	private class Graphlets : public WarGrey::SCADA::Planet {
	public:
		~Graphlets() noexcept;
		Graphlets(Platform::String^ plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		void on_tap(IGraphlet* snip, float x, float y, bool shifted, bool ctrled) override;

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
