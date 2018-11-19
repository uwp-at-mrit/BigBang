#pragma once

namespace WarGrey::SCADA {
	// DB300, starts from 1
	static unsigned int left_shifting_command  = 559U;
	static unsigned int right_shifting_command = 560U;

	static unsigned int close_all_butterfly_valves = 303U;
	static unsigned int stop_all_butterfly_valves = 806U;

	template<typename OP>
	uint16 DO_water_pump_group_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::S2_PS: index = 295U; break;
		case OP::S2_SB: index = 296U; break;
		case OP::S2_2:  index = 297U; break;
		case OP::P2_2:  index = 298U; break;
		case OP::S2_H:  index = 302U; break;
		case OP::P2_H:  index = 301U; break;
		case OP::I2_2:  index = 304U; break;
		}

		return index;
	}

	template<typename OP>
	uint16 DO_ps_water_pump_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::Prepare: index = 281U; break;
		case OP::Start:   index = 282U; break;
		case OP::Stop:    index = 283U; break;
		case OP::Reset:   index = 284U; break;
		case OP::PS_PS:   index = 289U; break;
		case OP::PS_SB:   index = 290U; break;
		case OP::PS_2:    index = 293U; break;
		case OP::PS_H:    index = 299U; break;
		default: index = DO_water_pump_group_command(cmd);
		}

		return index;
	}

	template<typename OP>
	uint16 DO_sb_water_pump_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::Prepare: index = 285U; break;
		case OP::Start:   index = 286U; break;
		case OP::Stop:    index = 287U; break;
		case OP::Reset:   index = 288U; break;
		case OP::SB_PS:   index = 291U; break;
		case OP::SB_SB:   index = 292U; break;
		case OP::SB_2:    index = 294U; break;
		case OP::SB_H:    index = 300U; break;
		default: index = DO_water_pump_group_command(cmd);
		}

		return index;
	}
}
