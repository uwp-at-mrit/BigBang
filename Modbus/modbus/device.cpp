#include "modbus/device.hpp"
#include "modbus/protocol.hpp"

using namespace WarGrey::SCADA;

int ModbusVirtualDevice::read_coils(uint16 address, uint16 quantity, uint8* dest) { // MAP: Page 10
    return -MODBUS_EXN_DEVICE_BUSY;
}

int ModbusVirtualDevice::write_coil(uint16 address, bool value) { // MAP: Page 10
    return 0;
}

int ModbusVirtualDevice::write_coils(uint16 address, uint16 quantity, uint8* dest) { // MAP: Page 10
    return -MODBUS_EXN_MEMORY_PARITY_ERROR;
}
