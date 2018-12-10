#include "plc.hpp"
#include "iotables/di_dredges.hpp"

using namespace WarGrey::SCADA;

template<class G>
static void _DI_gantry(G* target, const uint8* db4, unsigned int idx4, const uint8* db205, unsigned int idx205) {
	if (DBX(db4, idx4 + 0U)) {
		target->set_state(GantryState::WindedOut);
	} else if (DBX(db4, idx4 + 1U)) {
		target->set_state(GantryState::WindedUp);
	} else if (DBX(db205, idx205 + 0U)) {
		target->set_state(GantryState::WindingOut);
	} else if (DBX(db205, idx205 + 1U)) {
		target->set_state(GantryState::WindingUp);
	} else {
		target->set_state(GantryState::Default);
	}
}

/*************************************************************************************************/
void WarGrey::SCADA::DI_gantry(Gantrylet* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1) {
	_DI_gantry(target, db4, idx4_p1 - 1U, db205, idx205_p1 - 1U);
}

void WarGrey::SCADA::DI_gantry(GantrySymbollet* target, const uint8* db4, unsigned int idx4_p1, const uint8* db205, unsigned int idx205_p1) {
	_DI_gantry(target, db4, idx4_p1 - 1U, db205, idx205_p1 - 1U);
}

void WarGrey::SCADA::DI_suction_buttons(Buttonlet* intarget, Buttonlet* detarget, const uint8* db205, unsigned int idx_p1) {
	if (DBX(db205, idx_p1 - 1U)) {
		intarget->set_state(ButtonState::Executing);
	} else if (DBX(db205, idx_p1 + 1U)) {
		intarget->set_state(ButtonState::Failed);
	} else if (DBX(db205, idx_p1 + 3U)) {
		intarget->set_state(ButtonState::Ready);
	} else {
		intarget->set_state(ButtonState::Default);
	}

	if (DBX(db205, idx_p1 + 0U)) {
		detarget->set_state(ButtonState::Executing);
	} else if (DBX(db205, idx_p1 + 2U)) {
		detarget->set_state(ButtonState::Failed);
	} else if (DBX(db205, idx_p1 + 4U)) {
		detarget->set_state(ButtonState::Ready);
	} else {
		detarget->set_state(ButtonState::Default);
	}
}

void WarGrey::SCADA::DI_boolean_button(Buttonlet* target, const uint8* db205, unsigned idx_p1) {
	target->set_state(DBX(db205, idx_p1 - 1U), ButtonState::Executing, ButtonState::Default);
}

bool WarGrey::SCADA::DI_long_sb_drag(const uint8* db205) {
	return DBX(db205, 1417U - 1U);
}
