#pragma once

#include "plc.hpp"

#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"

namespace WarGrey::SCADA {
	private enum class GearboxLubricator { Master, Spare, _ };

	private enum class PSHopperPumpChargeAction { Prepare, Start, Stop, Reset, PSHopper, BothHopper, _ };
	private enum class SBHopperPumpChargeAction { Prepare, Start, Stop, Reset, SBHopper, BothHopper, HPBarge, _ };
	private enum class PSUnderWaterPumpChargeAction { Prepare, Start, Stop, Reset, PSUnderWater, BothUnderWater, _ };
	private enum class SBUnderWaterPumpChargeAction { Prepare, Start, Stop, Reset, SBUnderWater, BothUnderWater, UWPBarge, _ };
	private enum class PSHopperPumpDischargeAction { Prepare, Start, Stop, Reset, PSShoring, PSRainbowing, BothShoring, BothRainbowing, _ };
	private enum class SBHopperPumpDischargeAction { Prepare, Start, Stop, Reset, SBShoring, SBRainbowing, BothShoring, BothRainbowing, _ };

	private enum class GlandPumpAction { Start, Stop, Reset, Auto, _ };
	private enum class LubricationUnitAction { Start, Stop, _ };
	private enum class GearboxLubricatorAction { Start, Stop, Cancel, Auto, _ };

	typedef uint16(*gland_pump_action_f)(WarGrey::SCADA::GlandPumpAction, WarGrey::SCADA::HydraulicPumplet*);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_gland_pump_menu(WarGrey::SCADA::gland_pump_action_f gpc, WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_lubrication_unit_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_gearbox_lubricator_menu(WarGrey::SCADA::PLCMaster* plc);

	Windows::UI::Xaml::Controls::MenuFlyout^ make_ps_hopper_pump_charge_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_sb_hopper_pump_charge_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_ps_underwater_pump_charge_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_sb_underwater_pump_charge_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_ps_hopper_pump_discharge_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_sb_hopper_pump_discharge_menu(WarGrey::SCADA::PLCMaster* plc);

	// DB300, starts from 1
	template<typename E>
	uint16 DO_gland_pump_command(GlandPumpAction cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case GlandPumpAction::Reset: offset = 0U; break;
		case GlandPumpAction::Start: offset = 1U; break;
		case GlandPumpAction::Stop:  offset = 2U; break;
		case GlandPumpAction::Auto:  offset = 3U; break;
		}

		switch (id) {
		case E::PSHPa: index = 737U; break;
		case E::PSHPb: index = 741U; break;
		case E::SBHPa: index = 745U; break;
		case E::SBHPb: index = 749U; break;
		case E::PSUWP1: index = 753U; break;
		case E::PSUWP2: index = 757U; break;
		case E::SBUWP1: index = 761U; break;
		case E::SBUWP2: index = 765U; break;
		}

		return index + offset;
	}

	template<typename E>
	uint16 DO_gate_flushing_pump_command(GlandPumpAction cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case GlandPumpAction::Start: offset = 0U; break;
		case GlandPumpAction::Stop:  offset = 1U; break;
		case GlandPumpAction::Reset: offset = 2U; break;
		case GlandPumpAction::Auto:  offset = 3U; break;
		}

		switch (id) {
		case E::PSFP: index = 721U; break;
		case E::SBFP: index = 725U; break;
		}

		return index + offset;
	}
}
