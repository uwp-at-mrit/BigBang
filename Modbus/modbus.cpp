#include "modbus.hpp"

using namespace WarGrey::SCADA;

static const uint16 ninbit      = 0x16;
static const uint16 ninregister = 0x01;

static uint8 inbits_src[]       = { 0xAC, 0xDB, 0x35 };
static uint16 inregisters_src[] = { 0x0A };

IModbusServer* make_modbus_test_server() {
	auto device = new ModbusVirtualDevice(0x130, 0x25, 0x1C4, ninbit, 0x160, 0x20, 0x108, ninregister);
	
	device->initialize_discrete_inputs(0, ninbit, inbits_src);
	device->initialize_input_registers(0, ninregister, inregisters_src);
	device->enable_debug(true);

	return device;
}
