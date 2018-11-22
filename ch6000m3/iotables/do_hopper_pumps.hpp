#pragma once

#include "plc.hpp"

#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"

namespace WarGrey::SCADA {
	private enum class GlandPumpAction { Start, Stop, Reset, Auto, _ };

	typedef uint16(*gland_pump_action_f)(WarGrey::SCADA::GlandPumpAction, WarGrey::SCADA::HydraulicPumplet*);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_gland_pump_menu(WarGrey::SCADA::gland_pump_action_f gpc, WarGrey::SCADA::PLCMaster* plc);
	
	// DB300, starts from 1
	static unsigned int both_shoring  = 315U;
	static unsigned int both_rainbowing = 318U;

	template<typename OP>
	uint16 DO_hopper_pump_common_command(OP cmd, bool ps, bool hopper) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Prepare: offset = 0U; break;
		case OP::Start:   offset = 1U; break;
		case OP::Stop:    offset = 2U; break;
		case OP::Reset:   offset = 3U; break;
		}

		if (ps) {
			index = (hopper ? 325U : 321U);
		} else {
			index = (hopper ? 333U : 329U);
		}

		return index + offset;
	}

	template<typename OP>
	uint16 DO_ps_hopper_pump_charge_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::PSHopper:   index = 308U; break;
		case OP::BothHopper: index = 310U; break;
		default: index = DO_hopper_pump_common_command(cmd, true, true);
		}

		return index;
	}

	template<typename OP>
	uint16 DO_sb_hopper_pump_charge_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::SBHopper:       index = 309U; break;
		case OP::BothHopper:     index = 310U; break;
		case OP::HPBarge:        index = 311U; break;
		default: index = DO_hopper_pump_common_command(cmd, false, true);
		}

		return index;
	}

	template<typename OP>
	uint16 DO_ps_hopper_pump_discharge_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::PSDischarge:    index = 313U; break;
		case OP::PSRainbowing:   index = 316U; break;
		case OP::BothDischarge:  index = both_shoring; break;
		case OP::BothRainbowing: index = both_rainbowing; break;
		default: index = DO_hopper_pump_common_command(cmd, true, true);
		}

		return index;
	}

	template<typename OP>
	uint16 DO_sb_hopper_pump_discharge_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::SBDischarge:    index = 314U; break;
		case OP::SBRainbowing:   index = 317U; break;
		case OP::BothDischarge:  index = both_shoring; break;
		case OP::BothRainbowing: index = both_rainbowing; break;
		default: index = DO_hopper_pump_common_command(cmd, false, true);
		}

		return index;
	}

	template<typename OP>
	uint16 DO_ps_underwater_pump_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::PSUnderWater:   index = 305U; break;
		case OP::BothUnderWater: index = 307U; break;
		default: index = DO_hopper_pump_common_command(cmd, true, false);
		}

		return index;
	}

	template<typename OP>
	uint16 DO_sb_underwater_pump_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::SBUnderWater:   index = 306U; break;
		case OP::BothUnderWater: index = 307U; break;
		case OP::UWPBarge:       index = 312U; break;
		default: index = DO_hopper_pump_common_command(cmd, false, false);
		}

		return index;
	}

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

	template<typename OP>
	uint16 DO_hopper_lubricating_unit_command(OP cmd, bool ps) {
		uint16 offset = 0U;
		uint16 index = (ps ? 673U : 675U);

		switch (cmd) {
		case OP::Start: offset = 0U; break;
		case OP::Stop:  offset = 1U; break;
		}

		return index + offset;
	}

	template<typename OP, typename E>
	uint16 DO_hopper_gearbox_command(OP cmd, E id, bool ps) {
		uint16 index = (ps ? 505U : 513U);
		uint16 cmdoff = 0U;
		uint16 idoff = 0U;

		switch (cmd) {
		case OP::Start: cmdoff = 0U; break;
		case OP::Stop:  cmdoff = 1U; break;
		case OP::Auto:  cmdoff = 3U; break;
		}

		switch (id) {
		case E::Master: idoff = 0U; break;
		case E::Spare:  idoff = 4U; break;
		}

		return index + cmdoff + idoff;
	}
}
