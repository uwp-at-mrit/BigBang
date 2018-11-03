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

	template<class G>
	void DI_gate_valve(G* target, const uint8* db4, size_t idx_p1) {
		target->set_status(DBX(db4, idx_p1 - 1), GateValveStatus::Open, GateValveStatus::Closed);
	}

	template<class V>
	void DI_gate_valve(V* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		target->set_status(DBX(db4, idx4_p1 - 1), GateValveStatus::Open);
		target->set_status(DBX(db4, idx4_p1 + 0), GateValveStatus::Closed);

		target->set_status(DBX(db205, idx205_p1 - 1), GateValveStatus::Opening);
		target->set_status(DBX(db205, idx205_p1 + 0), GateValveStatus::Closing);
		target->set_status(DBX(db205, idx205_p1 + 1), GateValveStatus::Unopenable);
		target->set_status(DBX(db205, idx205_p1 + 2), GateValveStatus::Unclosable);
		target->set_status(DBX(db205, idx205_p1 + 3), GateValveStatus::OpenReady);
		target->set_status(DBX(db205, idx205_p1 + 4), GateValveStatus::CloseReady);
		target->set_status(DBX(db205, idx205_p1 + 5), GateValveStatus::FakeOpen);
		target->set_status(DBX(db205, idx205_p1 + 6), GateValveStatus::FakeClose);
	}

	template<class G, class M, typename E>
	void DI_paired_valves(G& gs, M& ms, E id
		, const uint8* db4, size_t gidx4_p1, size_t midx4_p1
		, const uint8* db205, size_t gidx205_p1, size_t midx205_p1) {
		auto gtarget = gs[id];

		gtarget->set_status(DBX(db4, gidx4_p1 - 1), GateValveStatus::Open);
		gtarget->set_status(DBX(db4, gidx4_p1 + 0), GateValveStatus::Closed);
		
		gtarget->set_status(DBX(db205, gidx205_p1 - 1), GateValveStatus::Opening);
		gtarget->set_status(DBX(db205, gidx205_p1 + 0), GateValveStatus::Closing);
		gtarget->set_status(DBX(db205, gidx205_p1 + 1), GateValveStatus::Unopenable);
		gtarget->set_status(DBX(db205, gidx205_p1 + 2), GateValveStatus::Unclosable);
		gtarget->set_status(DBX(db205, gidx205_p1 + 3), GateValveStatus::OpenReady);
		gtarget->set_status(DBX(db205, gidx205_p1 + 4), GateValveStatus::CloseReady);
		gtarget->set_status(DBX(db205, gidx205_p1 + 5), GateValveStatus::FakeOpen);
		gtarget->set_status(DBX(db205, gidx205_p1 + 6), GateValveStatus::FakeClose);

		if (ms.find(id) != ms.end()) {
			ms[id]->set_status(DBX(db4, midx4_p1 - 1), TValveStatus::Open);
			ms[id]->set_status(DBX(db4, midx4_p1 + 0), TValveStatus::Closed);
		}
	}
}
