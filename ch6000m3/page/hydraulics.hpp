#pragma once

#include "planet.hpp"
#include "plc.hpp"

#include "decorator/grid.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class HydraulicsPage : public WarGrey::SCADA::Planet {
	public:
		~HydraulicsPage() noexcept;
		HydraulicsPage(WarGrey::SCADA::PLCMaster* plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		bool can_select(IGraphlet* g) override;
		bool can_select_multiple() override;
		void on_tap_selected(IGraphlet* g, float local_x, float local_y) override;
		void on_gesture(std::list<Windows::Foundation::Numerics::float2>& anchors, float x, float y);

	private:
		WarGrey::SCADA::PLCMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;
		Windows::UI::Xaml::Controls::MenuFlyout^ gbs_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ gps_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ gsb_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ gvisor_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ pump_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ heater_op;

	private: // never deletes these graphlets mannually
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;

	private:
		WarGrey::SCADA::GridDecorator* grid;
	};
}
