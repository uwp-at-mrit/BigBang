#pragma once

#include <cinttypes>

#include "sockexn.hpp"

uint8 modbus_illegal_function(uint8 function_code, bool debug = true);
uint8 modbus_illegal_address(uint16 address, uint16 start, uint16 amount, bool debug = true);
uint8 modbus_illegal_address(uint16 address, uint16 count, uint16 start, uint16 amount, bool debug = true);

uint8 modbus_identification_not_found(uint16 id, bool debug = true);
uint8 modbus_identification_not_found(uint16 id, uint8 start, uint8 end, bool debug = true);
uint8 modbus_identification_not_found(uint16 id, uint8 start, uint8 end, uint8 product, bool debug = true);

uint8 modbus_illegal_data_value(uint16 value, uint16 vexpected, bool debug = true);
uint8 modbus_illegal_data_value(uint16 value, uint16 vmin, uint16 vmax, bool debug = true);
uint8 modbus_illegal_enum_value(uint16 value, uint16 v1, uint16 v2, bool debug = true);
