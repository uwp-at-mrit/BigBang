#pragma once

#include "planet.hpp"
#include "plc.hpp"

#include "decorator/grid.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class LoadingsPage : public WarGrey::SCADA::Planet {
	public:
		~LoadingsPage() noexcept;
		LoadingsPage(WarGrey::SCADA::PLCMaster* plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		bool can_select(IGraphlet* g) override;
		void on_tap_selected(IGraphlet* g, float x, float y) override;

	private:
		WarGrey::SCADA::PLCMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;
		Windows::UI::Xaml::Controls::MenuFlyout^ gate_valve_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ motor_valve_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ ps_hopper_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ sb_hopper_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ ps_underwater_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ sb_underwater_op;

	private: // never deletes these graphlets mannually
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;

	private:
		WarGrey::SCADA::GridDecorator* grid;
	};
}
