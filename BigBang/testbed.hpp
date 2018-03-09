#pragma once

#include "planet.hpp"
#include "modbus.hpp"
#include "command.hpp"

#include "snip/togglet.hpp"
#include "snip/textlet.hpp"
#include "snip/statuslet.hpp"
#include "snip/pumplet.hpp"
#include "snip/gaugelet.hpp"
#include "snip/shapelet.hpp"

namespace WarGrey::SCADA {
	private class Testbed : public WarGrey::SCADA::Planet {
	public:
		Testbed();

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;
	};
}
