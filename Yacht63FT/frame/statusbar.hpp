#pragma once

#include "planet.hpp"
#include "plc.hpp"

namespace WarGrey::SCADA {
	private class Statusbar : public WarGrey::SCADA::Planet {
	public:
		virtual ~Statusbar() noexcept;
		Statusbar(WarGrey::SCADA::IMRMaster* device);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void on_tap(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y) override;

	private:
		WarGrey::SCADA::IMRMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;
	};
}
