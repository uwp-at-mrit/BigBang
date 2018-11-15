#pragma once

#include "graphlet/device/winchlet.hpp"

namespace WarGrey::SCADA {
	// DB300, starts from 1
	template<typename OP, typename E>
	uint16 DO_winch_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Up:        offset = 0U; break;
		case OP::Down:      offset = 1U; break;
		case OP::Stop:      offset = 2U; break;
		case OP::HighSpeed: offset = 3U; break;
		}

		switch (id) {
		case E::psTrunnion:     index = 570U; break;
		case E::psIntermediate: index = 573U; break;
		case E::psDragHead:     index = 576U; break;
		case E::sbTrunnion:     index = 589U; break;
		case E::sbIntermediate: index = 592U; break;
		case E::sbDragHead:     index = 595U; break;
		}

		return index + offset;
	}
	
	template<typename OP, typename E>
	uint16 DO_gantry_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::WindOut: offset = 0U; break;
		case OP::WindUp:  offset = 1U; break;
		case OP::Stop:    offset = 2U; break;
		}

		switch (id) {
		case E::psTrunnion:     index = 561U; break;
		case E::psIntermediate: index = 564U; break;
		case E::psDragHead:     index = 567U; break;
		case E::sbTrunnion:     index = 580U; break;
		case E::sbIntermediate: index = 583U; break;
		case E::sbDragHead:     index = 586U; break;
		}

		return index + offset;
	}

	template<typename OP, typename E>
	uint16 DO_gantry_group_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::WindOut: offset = 0U; break;
		case OP::WindUp:  offset = 1U; break;
		case OP::Stop:    offset = 2U; break;
		}

		switch (id) {
		case E::PSGantries: index = 601U; break;
		case E::SBGantries: index = 604U; break;
		}

		return index + offset;
	}

	template<typename E>
	uint16 DO_gantry_virtual_action_command(E id, bool ps) {
		uint16 index = 0U;

		if (ps) {
			switch (id) {
			case E::tVirtualUp:  index = 793U; break;
			case E::tVirtualOut: index = 794U; break;
			case E::iVirtualUp:  index = 795U; break;
			case E::iVirtualOut: index = 796U; break;
			case E::hVirtualUp:  index = 797U; break;
			case E::hVirtualOut: index = 798U; break;
			}
		} else {
			switch (id) {
			case E::tVirtualUp:  index = 799U; break;
			case E::tVirtualOut: index = 800U; break;
			case E::iVirtualUp:  index = 801U; break;
			case E::iVirtualOut: index = 802U; break;
			case E::hVirtualUp:  index = 803U; break;
			case E::hVirtualOut: index = 804U; break;
			}
		}

		return index;
	}

	template<typename E>
	uint16 DO_wave_compensator_command(E cmd, bool ps) {
		uint16 offset = 0U;
		uint16 index = (ps ? 633U : 641U);

		switch (cmd) {
		case E::Charge:    offset = 0U; break;
		case E::Discharge: offset = 1U; break;
		case E::Stop:      offset = 2U; break;
		//case E::Lock:      offset = 3U; break;
		//case E::Unlock:    offset = 4U; break;
		}

		return index + offset;
	}

	template<typename E>
	uint16 DO_visor_command(E cmd, bool ps) {
		uint16 offset = 0U;
		uint16 index = (ps ? 649U : 652U);

		switch (cmd) {
		case E::Up:   offset = 0U; break;
		case E::Down: offset = 1U; break;
		case E::Stop: offset = 2U; break;
		}

		return index + offset;
	}

	template<typename CMD>
	uint16 DO_suction_command(CMD cmd, bool ps) {
		uint16 index = (ps ? 497U : 501U);
		uint16 offset = 0U;

		switch (cmd) {
		case CMD::Inflate: offset = 0U; break;
		case CMD::Deflate: offset = 1U; break;
		}

		return index + offset;
	}

	template<typename CMD>
	uint16 DO_LMOD_command(CMD cmd) {
		uint16 index = 881U;
		uint16 offset = 0U;

		switch (cmd) {
		case CMD::Emit: offset = 0U; break;
		case CMD::Fill: offset = 1U; break;
		case CMD::Auto: offset = 2U; break;
		}

		return index + offset;
	}
}
