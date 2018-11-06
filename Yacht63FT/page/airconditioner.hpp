#pragma once

#include "planet.hpp"
#include "plc.hpp"
#include "sqlite3.hpp"

#include "decorator/table.hpp"

namespace WarGrey::SCADA {
	private class ACPage : public WarGrey::SCADA::Planet {
	public:
		~ACPage() noexcept;
		ACPage(PLCMaster* device, Platform::String^ name);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;
		void on_tap(IGraphlet* g, float local_x, float local_y) override;

	private:
		WarGrey::SCADA::PLCMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;
		WarGrey::SCADA::PLCConfirmation* satellite;
		WarGrey::SCADA::TableDecorator* decorator;
	};
}
