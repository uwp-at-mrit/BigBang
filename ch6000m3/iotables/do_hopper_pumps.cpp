#include "menu.hpp"

#include "iotables/do_hopper_pumps.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;

private class GlandPumpExecutor final : public IMenuCommand<GlandPumpAction, HydraulicPumplet, PLCMaster*> {
public:
	GlandPumpExecutor(gland_pump_action_f gpc) : DO_action(gpc) {}

public:
	bool can_execute(GlandPumpAction cmd, HydraulicPumplet* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected();
	}

	void execute(GlandPumpAction cmd, HydraulicPumplet* pump, PLCMaster* plc) {
		plc->send_command(this->DO_action(cmd, pump));
	}

private:
	gland_pump_action_f DO_action;
};

/*************************************************************************************************/
MenuFlyout^ WarGrey::SCADA::make_gland_pump_menu(gland_pump_action_f gpc, PLCMaster* plc) {
	GlandPumpExecutor* exe = new GlandPumpExecutor(gpc);

	return make_menu<GlandPumpAction, HydraulicPumplet, PLCMaster*>(exe, plc);
}
