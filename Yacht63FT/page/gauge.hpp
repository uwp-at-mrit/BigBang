#pragma once

#include "planet.hpp"
#include "plc.hpp"
#include "sqlite3.hpp"

namespace WarGrey::SCADA {
	private class GaugePage : public WarGrey::SCADA::Planet {
	public:
		~GaugePage() noexcept;
		GaugePage(PLCMaster* device, Platform::String^ name);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void on_tap(IGraphlet* g, float local_x, float local_y) override;

	private:
		WarGrey::SCADA::PLCMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;
	};
}
