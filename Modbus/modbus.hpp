#pragma once

#include "modbus/device.hpp"
#include "modbus/client.hpp"
#include "modbus/constants.hpp"

WarGrey::SCADA::IModbusServer* make_modbus_test_server();
WarGrey::SCADA::IModbusClient* make_modbus_test_client(Platform::String^ device);
