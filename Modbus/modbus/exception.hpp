#pragma once

#include <cinttypes>

uint8 modbus_illegal_function(uint8 function_code, bool debug = true);
uint8 modbus_illegal_address(uint16 address, uint16 start, uint16 amount, bool debug = true);

uint8 modbus_illegal_data_value(uint16 value, uint16 vexpected, bool debug = true);
uint8 modbus_illegal_data_value(uint16 value, uint16 vmin, uint16 vmax, bool debug = true);
uint8 modbus_illegal_bool_value(uint16 value, uint16 vtrue, uint16 vfalse, bool debug = true);
