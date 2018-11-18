#pragma once

#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"

namespace WarGrey::SCADA {
	// DB300, starts from 1
	
	template<typename OP, typename E>
	uint16 DO_hopper_door_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Open:    offset = 0U; break;
		case OP::Stop:    offset = 1U; break;
		case OP::Close:   offset = 2U; break;
		case OP::Disable: offset = 3U; break;
		}

		switch (id) {
		case E::PS1: index = 337U; break;
		case E::SB1: index = 341U; break;
		case E::PS2: index = 345U; break;
		case E::SB2: index = 349U; break;
		case E::PS3: index = 353U; break;
		case E::SB3: index = 357U; break;
		case E::PS4: index = 361U; break;
		case E::SB4: index = 365U; break;
		case E::PS5: index = 369U; break;
		case E::SB5: index = 373U; break;
		case E::PS6: index = 377U; break;
		case E::SB6: index = 381U; break;
		case E::PS7: index = 385U; break;
		case E::SB7: index = 389U; break;
		}

		return index + offset;
	}

	template<typename OP, typename E>
	uint16 DO_hopper_door_group_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Open:  offset = 0U; break;
		case OP::Stop:  offset = 1U; break;
		case OP::Close: offset = 2U; break; 
		}

		switch (id) {
		case E::HDoor12: index = 417U; break;
		case E::HDoor35: index = 420U; break;
		case E::HDoor67: index = 423U; break;
		case E::HDoor17: index = 426U; break;
		}

		return index + offset;
	}

	template<typename OP>
	uint16 DO_hopper_doors_locks_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::Auto:   index = 429U; break;
		case OP::Locked: index = 431U; break;
		}

		return index;
	}

	template<typename OP>
	uint16 DO_hopper_doors_checks_command(OP cmd) {
		uint16 index = 0U;

		switch (cmd) {
		case OP::OpenDoorCheck:  index = 777U; break;
		case OP::CloseDoorCheck: index = 778U; break;
		}

		return index;
	}

	template<typename OP, typename E>
	uint16 DO_upper_door_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Open:    offset = 0U; break;
		case OP::Close:   offset = 1U; break;
		case OP::Stop:    offset = 2U; break;
		case OP::Disable: offset = 3U; break;
		}

		switch (id) {
		case E::PS1: index = 433U; break;
		case E::SB1: index = 437U; break;
		case E::PS2: index = 441U; break;
		case E::SB2: index = 445U; break;
		case E::PS3: index = 449U; break;
		case E::SB3: index = 453U; break;
		case E::PS4: index = 457U; break;
		case E::SB4: index = 461U; break;
		case E::PS5: index = 465U; break;
		case E::SB5: index = 469U; break;
		case E::PS6: index = 473U; break;
		case E::SB6: index = 477U; break;
		case E::PS7: index = 481U; break;
		case E::SB7: index = 485U; break;
		}

		return index + offset;
	}
}
