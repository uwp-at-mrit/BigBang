#include "menu.hpp"
#include "credit.hpp"

#include "iotables/do_hopper_pumps.hpp"

#include "graphlet/symbol/pump/hopper_pumplet.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;

static unsigned int hopper_pump_reset_offset = 3U;

static unsigned int both_hopper_dredging = 310U;
static unsigned int both_underwater_dredging = 307U;
static unsigned int both_shoring = 315U;
static unsigned int both_rainbowing = 318U;

inline static uint16 DO_hopper_pump_common_command_index(bool ps, bool hopper) {
	unsigned int index = 0U;
	
	if (ps) {
		index = (hopper ? 325U : 321U);
	} else {
		index = (hopper ? 333U : 329U);
	}

	return index;
}

template<typename OP>
static uint16 DO_hopper_pump_common_command(OP cmd, bool ps, bool hopper) {
	uint16 index = DO_hopper_pump_common_command_index(ps, hopper);
	uint16 offset = 0U;
	
	switch (cmd) {
	case OP::Prepare: offset = 0U; break;
	case OP::Start:   offset = 1U; break;
	case OP::Stop:    offset = 2U; break;
	case OP::Reset:   offset = hopper_pump_reset_offset; break;
	}

	return index + offset;
}

private class GlandPumpExecutor final : public IMenuCommand<GlandPumpAction, HydraulicPumplet, PLCMaster*> {
public:
	GlandPumpExecutor(gland_pump_action_f gpc, gland_pump_diagnostics_f gpd)
		: DO_action(gpc), show_diagnostics(gpd) {}

public:
	bool can_execute(GlandPumpAction cmd, HydraulicPumplet* pump, PLCMaster* plc, bool acc_executable) override {
		bool diagnostics = (GlandPumpAction::Diagnostics == cmd);

		return plc->connected() && (plc->authorized() || diagnostics);
	}

	void execute(GlandPumpAction cmd, HydraulicPumplet* pump, PLCMaster* plc) {
		if (cmd == GlandPumpAction::Diagnostics) {
			show_diagnostics(pump, plc);
		} else {
			plc->send_command(this->DO_action(cmd, pump));
		}
	}

private:
	gland_pump_action_f DO_action;
	gland_pump_diagnostics_f show_diagnostics;
};

private class LubricationUnitExecutor final : public IMenuCommand<LubricationUnitAction, Credit<HydraulicPumplet, bool>, PLCMaster*> {
public:
	bool can_execute(LubricationUnitAction cmd, Credit<HydraulicPumplet, bool>* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected() && plc->authorized();
	}

	void execute(LubricationUnitAction cmd, Credit<HydraulicPumplet, bool>* pump, PLCMaster* plc) override {
		uint16 index = (pump->id ? 673U : 677U); // `pump->id` indicates PS or SB
		uint16 offset = 0U;

		switch (cmd) {
		case LubricationUnitAction::Start: offset = 0U; break;
		case LubricationUnitAction::Stop:  offset = 1U; break;
		}

		plc->send_command(index + offset);
	}
};

private class GearboxExecutor final
	: public IMenuCommand<GearboxLubricatorAction, GroupCredit<HydraulicPumplet, bool, GearboxLubricator>, PLCMaster*> {
public:
	bool can_execute(GearboxLubricatorAction cmd, GroupCredit<HydraulicPumplet, bool, GearboxLubricator>* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected() && plc->authorized();
	}

	void execute(GearboxLubricatorAction cmd, GroupCredit<HydraulicPumplet, bool, GearboxLubricator>* pump, PLCMaster* plc) override {
		uint16 index = (pump->gid ? 505U : 513U); // `pump->gid` indicates PS or SB
		uint16 cmdoff = 0U;
		uint16 idoff = 0U;

		switch (cmd) {
		case GearboxLubricatorAction::Start:  cmdoff = 0U; break;
		case GearboxLubricatorAction::Stop:   cmdoff = 1U; break;
		case GearboxLubricatorAction::Cancel: cmdoff = 2U; break;
		case GearboxLubricatorAction::Auto:   cmdoff = 3U; break;
		}

		switch (pump->id) {
		case GearboxLubricator::Master: idoff = 0U; break;
		case GearboxLubricator::Spare:  idoff = 4U; break;
		}

		plc->send_command(index + cmdoff + idoff);
	}
};

private class PSHopperPumpChargeExecutor final : public IMenuCommand<PSHopperPumpChargeAction, HopperPumplet, PLCMaster*> {
public:
	bool can_execute(PSHopperPumpChargeAction cmd, HopperPumplet* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected() && plc->authorized();
	}

	void execute(PSHopperPumpChargeAction cmd, HopperPumplet* pump, PLCMaster* plc) override {
		uint16 index = 0U;

		switch (cmd) {
		case PSHopperPumpChargeAction::PSHopper:   index = 308U; break;
		case PSHopperPumpChargeAction::BothHopper: index = both_hopper_dredging; break;
		default: index = DO_hopper_pump_common_command(cmd, true, true);
		}

		plc->send_command(index);
	}
};

private class SBHopperPumpChargeExecutor final : public IMenuCommand<SBHopperPumpChargeAction, HopperPumplet, PLCMaster*> {
public:
	bool can_execute(SBHopperPumpChargeAction cmd, HopperPumplet* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected() && plc->authorized();
	}

	void execute(SBHopperPumpChargeAction cmd, HopperPumplet* pump, PLCMaster* plc) override {
		uint16 index = 0U;

		switch (cmd) {
		case SBHopperPumpChargeAction::SBHopper:   index = 309U; break;
		case SBHopperPumpChargeAction::HPBarge:    index = 311U; break;
		case SBHopperPumpChargeAction::BothHopper: index = both_hopper_dredging; break;
		default: index = DO_hopper_pump_common_command(cmd, false, true);
		}

		plc->send_command(index);
	}
};

private class PSUnderWaterPumpChargeExecutor final : public IMenuCommand<PSUnderWaterPumpChargeAction, HopperPumplet, PLCMaster*> {
public:
	bool can_execute(PSUnderWaterPumpChargeAction cmd, HopperPumplet* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected() && plc->authorized();
	}

	void execute(PSUnderWaterPumpChargeAction cmd, HopperPumplet* pump, PLCMaster* plc) override {
		uint16 index = 0U;

		switch (cmd) {
		case PSUnderWaterPumpChargeAction::PSUnderWater:   index = 305U; break;
		case PSUnderWaterPumpChargeAction::BothUnderWater: index = both_underwater_dredging; break;
		default: index = DO_hopper_pump_common_command(cmd, true, false);
		}

		plc->send_command(index);
	}
};

private class SBUnderWaterPumpChargeExecutor final : public IMenuCommand<SBUnderWaterPumpChargeAction, HopperPumplet, PLCMaster*> {
public:
	bool can_execute(SBUnderWaterPumpChargeAction cmd, HopperPumplet* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected() && plc->authorized();
	}

	void execute(SBUnderWaterPumpChargeAction cmd, HopperPumplet* pump, PLCMaster* plc) override {
		uint16 index = 0U;

		switch (cmd) {
		case SBUnderWaterPumpChargeAction::SBUnderWater:   index = 306U; break;
		case SBUnderWaterPumpChargeAction::UWPBarge:       index = 312U; break;
		case SBUnderWaterPumpChargeAction::BothUnderWater: index = both_underwater_dredging; break;
		default: index = DO_hopper_pump_common_command(cmd, false, false);
		}

		plc->send_command(index);
	}
};

private class PSHopperPumpDischargeExecutor final : public IMenuCommand<PSHopperPumpDischargeAction, HopperPumplet, PLCMaster*> {
public:
	bool can_execute(PSHopperPumpDischargeAction cmd, HopperPumplet* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected() && plc->authorized();
	}

	void execute(PSHopperPumpDischargeAction cmd, HopperPumplet* pump, PLCMaster* plc) override {
		uint16 index = 0U;

		switch (cmd) {
		case PSHopperPumpDischargeAction::PSShoring:      index = 313U; break;
		case PSHopperPumpDischargeAction::PSRainbowing:   index = 316U; break;
		case PSHopperPumpDischargeAction::BothShoring:    index = both_shoring; break;
		case PSHopperPumpDischargeAction::BothRainbowing: index = both_rainbowing; break;
		default: index = DO_hopper_pump_common_command(cmd, true, true);
		}

		plc->send_command(index);
	}
};

private class SBHopperPumpDischargeExecutor final : public IMenuCommand<SBHopperPumpDischargeAction, HopperPumplet, PLCMaster*> {
public:
	bool can_execute(SBHopperPumpDischargeAction cmd, HopperPumplet* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected() && plc->authorized();
	}

	void execute(SBHopperPumpDischargeAction cmd, HopperPumplet* pump, PLCMaster* plc) override {
		uint16 index = 0U;

		switch (cmd) {
		case SBHopperPumpDischargeAction::SBShoring:      index = 314U; break;
		case SBHopperPumpDischargeAction::SBRainbowing:   index = 317U; break;
		case SBHopperPumpDischargeAction::BothShoring:    index = both_shoring; break;
		case SBHopperPumpDischargeAction::BothRainbowing: index = both_rainbowing; break;
		default: index = DO_hopper_pump_common_command(cmd, false, true);
		}

		plc->send_command(index);
	}
};

/*************************************************************************************************/
MenuFlyout^ WarGrey::SCADA::make_gland_pump_menu(gland_pump_action_f gpc, gland_pump_diagnostics_f gpd, PLCMaster* plc) {
	return make_menu<GlandPumpAction, HydraulicPumplet, PLCMaster*>(new GlandPumpExecutor(gpc, gpd), plc);
}

MenuFlyout^ WarGrey::SCADA::make_lubrication_unit_menu(PLCMaster* plc) {
	return make_menu<LubricationUnitAction, Credit<HydraulicPumplet, bool>, PLCMaster*>(new LubricationUnitExecutor(), plc);
}

MenuFlyout^ WarGrey::SCADA::make_gearbox_lubricator_menu(PLCMaster* plc) {
	auto exe = new GearboxExecutor();

	return make_menu<GearboxLubricatorAction, GroupCredit<HydraulicPumplet, bool, GearboxLubricator>, PLCMaster*>(exe, plc);
}

MenuFlyout^ WarGrey::SCADA::make_ps_hopper_pump_charge_menu(PLCMaster* plc) {
	return make_menu<PSHopperPumpChargeAction, HopperPumplet, PLCMaster*>(new PSHopperPumpChargeExecutor(), plc);
}

MenuFlyout^ WarGrey::SCADA::make_sb_hopper_pump_charge_menu(PLCMaster* plc) {
	return make_menu<SBHopperPumpChargeAction, HopperPumplet, PLCMaster*>(new SBHopperPumpChargeExecutor(), plc);
}

MenuFlyout^ WarGrey::SCADA::make_ps_underwater_pump_charge_menu(PLCMaster* plc) {
	return make_menu<PSUnderWaterPumpChargeAction, HopperPumplet, PLCMaster*>(new PSUnderWaterPumpChargeExecutor(), plc);
}

MenuFlyout^ WarGrey::SCADA::make_sb_underwater_pump_charge_menu(PLCMaster* plc) {
	return make_menu<SBUnderWaterPumpChargeAction, HopperPumplet, PLCMaster*>(new SBUnderWaterPumpChargeExecutor(), plc);
}

MenuFlyout^ WarGrey::SCADA::make_ps_hopper_pump_discharge_menu(PLCMaster* plc) {
	return make_menu<PSHopperPumpDischargeAction, HopperPumplet, PLCMaster*>(new PSHopperPumpDischargeExecutor(), plc);
}

MenuFlyout^ WarGrey::SCADA::make_sb_hopper_pump_discharge_menu(PLCMaster* plc) {
	return make_menu<SBHopperPumpDischargeAction, HopperPumplet, PLCMaster*>(new SBHopperPumpDischargeExecutor(), plc);
}

/*************************************************************************************************/
uint16 WarGrey::SCADA::DO_gate_flushing_pump_command(GlandPumpAction cmd, bool ps) { // DB300, starts from 1
	uint16 offset = 0U;
	uint16 index = (ps ? 721U : 725U);

	switch (cmd) {
	case GlandPumpAction::Start: offset = 0U; break;
	case GlandPumpAction::Stop:  offset = 1U; break;
	case GlandPumpAction::Reset: offset = 2U; break;
	case GlandPumpAction::Auto:  offset = 3U; break;
	}

	return index + offset;
}

uint16 WarGrey::SCADA::DO_gland_pump_command(GlandPumpAction cmd, bool ps, bool hopper, bool master) { // DB300, starts from 1
	uint16 offset = 0U;
	uint16 index = 0U;

	switch (cmd) {
	case GlandPumpAction::Reset: offset = 0U; break;
	case GlandPumpAction::Start: offset = 1U; break;
	case GlandPumpAction::Stop:  offset = 2U; break;
	case GlandPumpAction::Auto:  offset = 3U; break;
	}

	if (hopper) {
		if (ps) {
			index = (master ? 737U : 741U);
		} else {
			index = (master ? 745U : 749U);
		}
	} else {
		if (ps) {
			index = (master ? 753U : 757U);
		} else {
			index = (master ? 761U : 765U);
		}
	}

	return index + offset;
}


/*************************************************************************************************/
unsigned int WarGrey::SCADA::DO_hopper_pump_reset_command(bool ps, bool hopper) {
	return DO_hopper_pump_common_command_index(ps, hopper) + hopper_pump_reset_offset;
}
