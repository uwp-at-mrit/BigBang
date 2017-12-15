#pragma once

#include "modbus.hpp"

WarGrey::SCADA::IModbusServer* make_modbus_test_server();
WarGrey::SCADA::IModbusClient* make_modbus_test_client(Platform::String^ device, WarGrey::SCADA::IModbusConfirmation* confirmation);

void modbus_test_client(Platform::String^ device, bool debug = true);
void modbus_test_server();
