#pragma once

#include "graphlet/device/winchlet.hpp"

namespace WarGrey::SCADA {
	// DB300, starts from 1
	template<typename OP, typename E>
	uint16 DO_winch_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Up: offset = 0U; break;
		case OP::Down:  offset = 1U; break;
		case OP::Stop: offset = 2U; break;
		case OP::HighSpeed: offset = 3U; break;
		}

		switch (id) {
		case E::psTrunnion: index = 570U; break;
		case E::psIntermediate: index = 573U; break;
		case E::psDragHead: index = 576U; break;
		case E::sbTrunnion: index = 589U; break;
		case E::sbIntermediate: index = 592U; break;
		case E::sbDragHead: index = 595U; break;
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
		case OP::Stop: offset = 2U; break;
		}

		switch (id) {
		case E::psTrunnion: index = 561U; break;
		case E::psIntermediate: index = 564U; break;
		case E::psDragHead: index = 567U; break;
		case E::sbTrunnion: index = 580U; break;
		case E::sbIntermediate: index = 583U; break;
		case E::sbDragHead: index = 586U; break;
		}

		return index + offset;
	}
}
