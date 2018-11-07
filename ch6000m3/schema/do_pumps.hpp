#pragma once

#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"

namespace WarGrey::SCADA {
	// DB300, starts from 1
	template<typename OP, typename E>
	uint16 DO_hydraulic_pump_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Start: offset = 0U; break;
		case OP::Stop:  offset = 1U; break;
		case OP::Reset: offset = 2U; break;
		}

		switch (id) {
		case E::A: index = 9U; break;
		case E::B: index = 12U; break;
		case E::C: index = 15U; break;
		case E::D: index = 18U; break;
		case E::E: index = 21U; break;
		case E::F: index = 24U; break;
		case E::G: index = 27U; break;
		case E::H: index = 30U; break;
		case E::I: index = 36U; break;
		case E::J: index = 33U; break;
		case E::K: index = 39U; break;
		case E::L: index = 42U; break;
		case E::M: index = 45U; break;
		case E::Y: index = 48U; break;
		}

		return index + offset;
	}

	template<typename OP, typename E>
	uint16 DO_gate_flushing_pump_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Start: offset = 0U; break;
		case OP::Stop:  offset = 1U; break;
		case OP::Reset: offset = 2U; break;
		case OP::Auto:  offset = 3U; break;
		}

		switch (id) {
		case E::PSFP: index = 721U; break;
		case E::SBFP: index = 725U; break;
		}

		return index + offset;
	}

	template<typename OP, typename E>
	uint16 DO_sealed_water_pump_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Reset: offset = 0U; break;
		case OP::Start: offset = 1U; break;
		case OP::Stop:  offset = 2U; break;
		case OP::Auto:  offset = 3U; break;
		}

		switch (id) {
		case E::PSFP: index = 721U; break;
		case E::SBFP: index = 725U; break;
		}

		return index + offset;
	}
}
