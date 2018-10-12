#pragma once

#include <map>

#include "graphlet/symbol/valve/gate_valvelet.hpp"

namespace WarGrey::SCADA {
	template<class G>
	void DI_gate_valve(G* target, const uint8* db4, size_t idx_p1) {
		target->set_status(DBX(db4, idx_p1 - 1), GateValveStatus::Open, GateValveStatus::Closed);
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
