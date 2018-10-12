#pragma once

#include "graphlet/textlet.hpp"

namespace WarGrey::SCADA {
	template<class D>
	void DI_pump_dimension(D* target, const uint8* db4, size_t idx_p1) {
		target->set_status(DBX(db4, idx_p1 - 1) ? DimensionStatus::Highlight : DimensionStatus::Normal);
	}
}
