#pragma once

#include <map>

#include "graphlet/symbol/valve/gate_valvelet.hpp"

namespace WarGrey::SCADA {
	// DB4, starts from 1
	static unsigned int gate_valve_SQ1_status = 113U;
	static unsigned int gate_valve_SQ2_status = 114U;

	static unsigned int gate_valve_SQk1_status = 122U;
	static unsigned int gate_valve_SQk2_status = 123U;
	static unsigned int gate_valve_SQl_status = 124U;
	static unsigned int gate_valve_SQm_status = 125U;
	static unsigned int gate_valve_SQy_status = 126U;

	static unsigned int gate_valve_SQi_status = 128U;
	static unsigned int gate_valve_SQj_status = 129U;

	static unsigned int gate_valve_SQc_status = 135U;
	static unsigned int gate_valve_SQd_status = 136U;
	static unsigned int gate_valve_SQe_status = 137U;
	static unsigned int gate_valve_SQf_status = 138U;

	static unsigned int gate_valve_SQa_status = 141U;
	static unsigned int gate_valve_SQb_status = 142U;
	static unsigned int gate_valve_SQg_status = 143U;
	static unsigned int gate_valve_SQh_status = 144U;

	static unsigned int gate_valve_D01_feedback = 239U;
	static unsigned int gate_valve_D02_feedback = 273U;
	static unsigned int gate_valve_D03_feedback = 279U;
	static unsigned int gate_valve_D04_feedback = 257U;
	static unsigned int gate_valve_D05_feedback = 259U;
	static unsigned int gate_valve_D06_feedback = 261U;
	static unsigned int gate_valve_D07_feedback = 289U;
	static unsigned int gate_valve_D08_feedback = 291U;
	static unsigned int gate_valve_D09_feedback = 293U;
	static unsigned int gate_valve_D10_feedback = 295U;
	static unsigned int gate_valve_D11_feedback = 349U;
	static unsigned int gate_valve_D12_feedback = 333U;
	static unsigned int gate_valve_D13_feedback = 405U;
	static unsigned int gate_valve_D14_feedback = 373U;
	static unsigned int gate_valve_D15_feedback = 407U;
	static unsigned int gate_valve_D16_feedback = 375U;
	static unsigned int gate_valve_D17_feedback = 297U;
	static unsigned int gate_valve_D18_feedback = 299U;
	static unsigned int gate_valve_D19_feedback = 301U;
	static unsigned int gate_valve_D20_feedback = 303U;
	static unsigned int gate_valve_D21_feedback = 305U;
	static unsigned int gate_valve_D22_feedback = 307U;
	static unsigned int gate_valve_D23_feedback = 309U;
	static unsigned int gate_valve_D24_feedback = 413U;
	static unsigned int gate_valve_D25_feedback = 275U;
	static unsigned int gate_valve_D26_feedback = 277U;

	static unsigned int motor_valve_D01_feedback = 465U;
	static unsigned int motor_valve_D02_feedback = 421U;
	static unsigned int motor_valve_D03_feedback = 423U;
	static unsigned int motor_valve_D04_feedback = 425U;
	static unsigned int motor_valve_D05_feedback = 417U;
	static unsigned int motor_valve_D06_feedback = 419U;
	static unsigned int motor_valve_D07_feedback = 455U;
	static unsigned int motor_valve_D08_feedback = 457U;
	static unsigned int motor_valve_D09_feedback = 443U;
	static unsigned int motor_valve_D10_feedback = 439U;
	static unsigned int motor_valve_D11_feedback = 441U;
	static unsigned int motor_valve_D12_feedback = 437U;
	static unsigned int motor_valve_D13_feedback = 433U;
	static unsigned int motor_valve_D14_feedback = 431U;
	static unsigned int motor_valve_D15_feedback = 429U;
	static unsigned int motor_valve_D16_feedback = 427U;
	static unsigned int motor_valve_D17_feedback = 445U;
	static unsigned int motor_valve_D18_feedback = 453U;
	static unsigned int motor_valve_D19_feedback = 451U;
	static unsigned int motor_valve_D20_feedback = 447U;
	static unsigned int motor_valve_D21_feedback = 449U;
	static unsigned int motor_valve_D22_feedback = 461U;
	static unsigned int motor_valve_D23_feedback = 459U;
	static unsigned int motor_valve_D24_feedback = 435U;
	static unsigned int motor_valve_D25_feedback = 467U;
	static unsigned int motor_valve_D26_feedback = 463U;

	static unsigned int gate_valve_D01_status = 369U;
	static unsigned int gate_valve_D02_status = 393U;
	static unsigned int gate_valve_D03_status = 385U;
	static unsigned int gate_valve_D04_status = 377U;
	static unsigned int gate_valve_D05_status = 401U;
	static unsigned int gate_valve_D06_status = 409U;
	static unsigned int gate_valve_D07_status = 417U;
	static unsigned int gate_valve_D08_status = 425U;
	static unsigned int gate_valve_D09_status = 433U;
	static unsigned int gate_valve_D10_status = 441U;
	static unsigned int gate_valve_D11_status = 449U;
	static unsigned int gate_valve_D12_status = 457U;
	static unsigned int gate_valve_D13_status = 465U;
	static unsigned int gate_valve_D14_status = 473U;
	static unsigned int gate_valve_D15_status = 481U;
	static unsigned int gate_valve_D16_status = 489U;
	static unsigned int gate_valve_D17_status = 497U;
	static unsigned int gate_valve_D18_status = 505U;
	static unsigned int gate_valve_D19_status = 513U;
	static unsigned int gate_valve_D20_status = 521U;
	static unsigned int gate_valve_D21_status = 529U;
	static unsigned int gate_valve_D22_status = 537U;
	static unsigned int gate_valve_D23_status = 545U;
	static unsigned int gate_valve_D24_status = 553U;
	static unsigned int gate_valve_D25_status = 561U;
	static unsigned int gate_valve_D26_status = 569U;

	template<class G>
	void DI_gate_valve(G* target, const uint8* db4, size_t idx_p1) {
		target->set_status(DBX(db4, idx_p1 - 1), GateValveStatus::Open, GateValveStatus::Closed);
	}

	/*********************************************************************************************/
	template<class V>
	void DI_gate_valve(V* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		target->set_status(DBX(db4, idx4_p1 - 1U), GateValveStatus::Open);
		target->set_status(DBX(db4, idx4_p1 + 0U), GateValveStatus::Closed);

		target->set_status(DBX(db205, idx205_p1 - 1U), GateValveStatus::Opening);
		target->set_status(DBX(db205, idx205_p1 + 0U), GateValveStatus::Closing);
		target->set_status(DBX(db205, idx205_p1 + 1U), GateValveStatus::Unopenable);
		target->set_status(DBX(db205, idx205_p1 + 2U), GateValveStatus::Unclosable);
		target->set_status(DBX(db205, idx205_p1 + 3U), GateValveStatus::OpenReady);
		target->set_status(DBX(db205, idx205_p1 + 4U), GateValveStatus::CloseReady);
		target->set_status(DBX(db205, idx205_p1 + 5U), GateValveStatus::VirtualOpen);
		target->set_status(DBX(db205, idx205_p1 + 6U), GateValveStatus::VirtualClose);
	}

	template<class G, class M>
	void DI_paired_valves(G* gtarget, M* mtarget, const uint8* db4, size_t gidx4_p1, size_t midx4_p1, const uint8* db205, size_t gidx205_p1) {
		DI_gate_valve(gtarget, db4, gidx4_p1, db205, gidx205_p1);

		if (mtarget != nullptr) {
			mtarget->set_status(DBX(db4, midx4_p1 - 1U), TValveStatus::Open);
			mtarget->set_status(DBX(db4, midx4_p1 + 0U), TValveStatus::Closed);
		}
	}

	template<class G, typename Menu>
	bool gate_valve_command_executable(G* target, Menu cmd, bool otherwise) {
		GateValveStatus status = target->get_status();
		bool executable = otherwise;

		return executable;
	}

	template<class M, typename Menu>
	bool motor_valve_command_executable(M* target, Menu cmd, bool otherwise) {
		TValveStatus status = target->get_status();
		bool executable = otherwise;

		return executable;
	}
}
