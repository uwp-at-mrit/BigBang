#pragma once

#include "planet.hpp"
#include "satellite.hpp"
#include "plc.hpp"

#include "decorator/grid.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class ChargesPage : public WarGrey::SCADA::Planet {
	public:
		~ChargesPage() noexcept;
		ChargesPage(WarGrey::SCADA::PLCMaster* plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		bool can_select(IGraphlet* g) override;
		bool can_select_multiple() override;
		void on_tap_selected(IGraphlet* g, float x, float y) override;
		void on_gesture(std::list<Windows::Foundation::Numerics::float2>& anchors, float x, float y) override;

	private:
		WarGrey::SCADA::PLCMaster* device;
		WarGrey::SCADA::PLCConfirmation* dashboard;
		WarGrey::SCADA::ISatellite* diagnostics;
		Windows::UI::Xaml::Controls::MenuFlyout^ gate_valve_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ ghopper_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ gunderwater_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ ghbarge_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ guwbarge_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ ghps_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ ghsb_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ guwps_op;
		Windows::UI::Xaml::Controls::MenuFlyout^ guwsb_op;
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
