#pragma once

#include "universe.hpp"
#include "modbus.hpp"

#include "snip/textlet.hpp"
#include "snip/statuslet.hpp"
#include "snip/storagelet.hpp"
#include "snip/motorlet.hpp"
#include "snip/gaugelet.hpp"
#include "snip/vibratorlet.hpp"
#include "snip/liquidlet.hpp"
#include "snip/pipeline/funnellet.hpp"
#include "snip/pipeline/sleevelet.hpp"
#include "snip/pipeline/screwlet.hpp"
#include "snip/pipeline/gluecleanerlet.hpp"

namespace WarGrey::SCADA {
	private class BSegment : public WarGrey::SCADA::Universe {
	public:
		BSegment(Windows::UI::Xaml::Controls::Panel^ parent, Platform::String^ label, Platform::String^ plc);
		~BSegment() noexcept;

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesEventArgs^ args, float width, float height) override;
		void reflow(float width, float height) override;

	private:
		WarGrey::SCADA::IModbusConfirmation* console;
	};
}
