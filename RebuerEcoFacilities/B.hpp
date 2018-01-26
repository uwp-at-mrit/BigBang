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
#include "snip/serew/funnellet.hpp"
#include "snip/serew/sleevelet.hpp"
#include "snip/serew/gearboxlet.hpp"
#include "snip/serew/gluecleanerlet.hpp"

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
		Platform::String^ caption;
		Platform::String^ device;

	// never deletes these snips mannually	
	private:
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;
	};
}
