#include "menu.hpp"

#include "iotables/do_pumps.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;

private class HydraulicPumpExecutor final : public IMenuCommand<HydraulicPumpAction, HydraulicPumplet, PLCMaster*> {
public:
	HydraulicPumpExecutor(hydraulic_pump_action_f hpa) : DO_action(hpa) {}

public:
	bool can_execute(HydraulicPumpAction cmd, HydraulicPumplet* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected();
	}

	void execute(HydraulicPumpAction cmd, HydraulicPumplet* pump, PLCMaster* plc) override {
		plc->send_command(this->DO_action(cmd, pump));
	}

private:
	hydraulic_pump_action_f DO_action;
};

private class HydraulicPumpGroupExecutor final : public IGroupMenuCommand<HydraulicsGroupAction, HydraulicsGroup, PLCMaster*> {
public:
	bool can_execute(HydraulicsGroupAction cmd, HydraulicsGroup group, PLCMaster* plc) override {
		return plc->connected();
	}

	void execute(HydraulicsGroupAction cmd, HydraulicsGroup group, PLCMaster* plc) override { // DB300, starts from 1
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case HydraulicsGroupAction::Start:  offset = 0U; break;
		case HydraulicsGroupAction::Stop:   offset = 1U; break;
		case HydraulicsGroupAction::Cancel: offset = 2U; break;
		}

		switch (group) {
		case HydraulicsGroup::BothPumps:  index = 51U; break;
		case HydraulicsGroup::SBPumps:    index = 54U; break;
		case HydraulicsGroup::PSPumps:    index = 61U; break;
		case HydraulicsGroup::VisorPumps: index = 64U; break;
		}

		plc->send_command(index + offset);
	}
};

/*************************************************************************************************/
MenuFlyout^ WarGrey::SCADA::make_hydraulic_pump_menu(hydraulic_pump_action_f gpa, PLCMaster* plc) {
	return make_menu<HydraulicPumpAction, HydraulicPumplet, PLCMaster*>(new HydraulicPumpExecutor(gpa), plc);
}

MenuFlyout^ WarGrey::SCADA::make_hydraulics_group_menu(HydraulicsGroup group, PLCMaster* plc) {
	return make_group_menu<HydraulicsGroupAction, HydraulicsGroup, PLCMaster*>(new HydraulicPumpGroupExecutor(), group, plc);
}
