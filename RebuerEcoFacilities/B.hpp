#pragma once

#include "planet.hpp"
#include "modbus.hpp"

#include "snip/textlet.hpp"
#include "snip/statuslet.hpp"
#include "snip/storagelet.hpp"
#include "snip/motorlet.hpp"
#include "snip/gaugelet.hpp"
#include "snip/vibratorlet.hpp"
#include "snip/liquidlet.hpp"
#include "snip/screw/funnellet.hpp"
#include "snip/screw/sleevelet.hpp"
#include "snip/screw/gearboxlet.hpp"
#include "snip/screw/gluecleanerlet.hpp"

namespace WarGrey::SCADA {
	private class BSegment : public WarGrey::SCADA::Planet {
	public:
		BSegment(Platform::String^ label, Platform::String^ plc);
		~BSegment() noexcept;

	public:
		void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	private:
		WarGrey::SCADA::IModbusConfirmation* console;
	};
}
