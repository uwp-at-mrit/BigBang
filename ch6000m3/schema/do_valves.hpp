#pragma once

#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"

namespace WarGrey::SCADA {
	// DB300, starts from 1
	template<typename OP, typename E>
	uint16 DO_gate_valve_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Open:         offset = 0U; break;
		case OP::Close:        offset = 1U; break;
		case OP::VirtualOpen:  offset = 2U; break;
		case OP::VirtualClose: offset = 3U; break;
		}

		switch (id) {
		case E::D001: index = 73U; break;
		case E::D002: index = 85U; break;
		case E::D003: index = 81U; break;
		case E::D004: index = 77U; break;
		case E::D005: index = 89U; break;
		case E::D006: index = 93U; break;
		case E::D007: index = 97U; break;
		case E::D008: index = 101U; break;
		case E::D009: index = 105U; break;
		case E::D010: index = 109U; break;
		case E::D011: index = 113U; break;
		case E::D012: index = 117U; break;
		case E::D013: index = 121U; break;
		case E::D014: index = 125U; break;
		case E::D015: index = 129U; break;
		case E::D016: index = 133U; break;
		case E::D017: index = 137U; break;
		case E::D018: index = 141U; break;
		case E::D019: index = 145U; break;
		case E::D020: index = 149U; break;
		case E::D021: index = 153U; break;
		case E::D022: index = 157U; break;
		case E::D023: index = 161U; break;
		case E::D024: index = 165U; break;
		case E::D025: index = 169U; break;
		case E::D026: index = 173U; break;
		}

		return index + offset;
	}

	template<typename OP, typename E>
	uint16 DO_motor_valve_command(OP cmd, E id) {
		int16 offset = -1;
		uint16 index = 0U;
		uint16 heat = 0U;

		switch (cmd) {
		case OP::Open:         offset = 0U; break;
		case OP::Close:        offset = 1U; break;
		case OP::VirtualOpen:  offset = 2U; break;
		case OP::VirtualClose: offset = 3U; break;
		}

		switch (id) {
		case E::D001: index = 1017U; heat = 1121U; break;
		case E::D002: index = 1029U; heat = 1124U; break;
		case E::D003: index = 1025U; heat = 1123U; break;
		case E::D004: index = 1021U; heat = 1122U; break;
		case E::D005: index = 1033U; heat = 1125U; break;
		case E::D006: index = 1037U; heat = 1126U; break;
		case E::D007: index = 1041U; heat = 1127U; break;
		case E::D008: index = 1045U; heat = 1128U; break;
		case E::D009: index = 1049U; heat = 1129U; break;
		case E::D010: index = 1053U; heat = 1130U; break;
		case E::D011: index = 1057U; heat = 1131U; break;
		case E::D012: index = 1061U; heat = 1132U; break;
		case E::D013: index = 1065U; heat = 1133U; break;
		case E::D014: index = 1069U; heat = 1134U; break;
		case E::D015: index = 1073U; heat = 1135U; break;
		case E::D016: index = 1077U; heat = 1136U; break;
		case E::D017: index = 1081U; heat = 1137U; break;
		case E::D018: index = 1085U; heat = 1138U; break;
		case E::D019: index = 1089U; heat = 1139U; break;
		case E::D020: index = 1093U; heat = 1140U; break;
		case E::D021: index = 1097U; heat = 1141U; break;
		case E::D022: index = 1101U; heat = 1142U; break;
		case E::D023: index = 1105U; heat = 1143U; break;
		case E::D024: index = 1109U; heat = 1144U; break;
		case E::D025: index = 1113U; heat = 1145U; break;
		case E::D026: index = 1117U; heat = 1146U; break;
		}

		return ((offset >= 0U) ? (index + offset) : heat);
	}

	template<typename OP, typename E>
	uint16 DO_butterfly_valve_command(OP cmd, E id) {
		uint16 offset = 0U;
		uint16 index = 0U;

		switch (cmd) {
		case OP::Open:         offset = 0U; break;
		case OP::Close:        offset = 1U; break;
		case OP::VirtualOpen:  offset = 2U; break;
		case OP::VirtualClose: offset = 3U; break;
		}

		switch (id) {
		case E::HBV01: index = 185U; break;
		case E::HBV02: index = 189U; break;
		case E::HBV03: index = 193; break;
		case E::HBV04: index = 197U; break;
		case E::HBV05: index = 201U; break;
		case E::HBV06: index = 205U; break;
		case E::HBV07: index = 209U; break;
		case E::HBV08: index = 213U; break;
		case E::HBV09: index = 217U; break;
		case E::HBV10: index = 221U; break;
		case E::HBV11: index = 225U; break;
		case E::HBV12: index = 229U; break;
		case E::HBV13: index = 233U; break;
		case E::HBV14: index = 237U; break;
		case E::HBV15: index = 241U; break;
		case E::HBV16: index = 245U; break;
		case E::HBV17: index = 249U; break;
		case E::HBV18: index = 253U; break;
		}

		return index + offset;
	}
}
