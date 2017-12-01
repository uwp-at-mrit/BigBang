#include "modbus/exception.hpp"
#include "modbus/constants.hpp"
#include "rsyslog.hpp"

uint8 modbus_illegal_function(uint8 function_code, bool debug) {
	if (debug) {
		auto strfmt = L"[Unknown function code 0x%02X]";

		rsyslog(strfmt, function_code);
	}

	return MODBUS_EXN_ILLEGAL_FUNCTION;
}

uint8 modbus_illegal_address(uint16 address, uint16 start, uint16 amount, bool debug) {
	rsyslog(L"[Illegal data address 0x%04X, out of range [0x%04X, 0x%04X]]", address, start, start + amount);

	return MODBUS_EXN_ILLEGAL_DATA_ADDRESS;
}

uint8 modbus_illegal_data_value(uint16 value, uint16 vexpected, bool debug) {
    rsyslog(L"[Illegal data value, expected 0x%04X, given 0x%04X]", vexpected, value);

	return MODBUS_EXN_ILLEGAL_DATA_VALUE;
}

uint8 modbus_illegal_data_value(uint16 value, uint16 vmin, uint16 vmax, bool debug) {
    rsyslog(L"[Illegal data value 0x%04X, out of range [0x%04X, 0x%04X]]", value, vmin, vmax);

	return MODBUS_EXN_ILLEGAL_DATA_VALUE;
}

uint8 modbus_illegal_bool_value(uint16 value, uint16 vtrue, uint16 vfalse, bool debug) {
    rsyslog(L"[Illegal data value 0x%04X, out of range {0x%04X, 0x%04X}]", value, vtrue, vfalse);

	return MODBUS_EXN_ILLEGAL_DATA_VALUE;
}
