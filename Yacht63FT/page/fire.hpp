#pragma once

#include "planet.hpp"
#include "plc.hpp"

namespace WarGrey::SCADA {
	private class FirePage : public WarGrey::SCADA::Planet {
	public:
		~FirePage() noexcept;
		FirePage(PLCMaster* device, Platform::String^ name);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) override;

	private:
		WarGrey::SCADA::PLCMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;
	};
}
