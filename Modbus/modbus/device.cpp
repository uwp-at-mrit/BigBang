#include <algorithm>

#include "modbus/device.hpp"
#include "modbus/protocol.hpp"
#include "modbus/exception.hpp"
#include "rsyslog.hpp"

using namespace WarGrey::SCADA;

ModbusVirtualDevice::ModbusVirtualDevice(uint16 port) : ModbusVirtualDevice(0, 0, 0, 0) {};

ModbusVirtualDevice::ModbusVirtualDevice(uint16 nbits, uint16 ninbits, uint16 nregisters, uint16 ninregisters, uint16 port)
    : ModbusVirtualDevice(0, nbits, 0, ninbits, 0, nregisters, 0, ninregisters) {}

ModbusVirtualDevice::ModbusVirtualDevice(uint16 bit0, uint16 nbits, uint16 inbit0, uint16 ninbits
    , uint16 register0, uint16 nregisters, uint16 inregister0, uint16 ninregisters, uint16 port) : IModbusServer(port) {
    uint16 omit = std::min(std::min(bit0, inbit0), std::min(register0, inregister0));
    size_t bits = std::max(bit0 + nbits, inbit0 + ninbits) * sizeof(uint8);
    size_t registers = std::max(register0 + nregisters * sizeof(uint16), inregister0 + ninregisters * sizeof(uint16));

    this->memory = (uint8*)calloc(std::max(bits, registers) - omit, sizeof(uint8));
    if (this->memory != nullptr) {
        this->bit0 = bit0;
        this->nbits = nbits;
        this->coils = (uint8*)(this->memory + (bit0 - omit));
    
        this->inbit0 = inbit0;
        this->ninbits = ninbits;
        this->discrete_inputs = (uint8*)(this->memory + (inbit0 - omit));
    
        this->register0 = register0;
        this->nregisters = nregisters;
        this->holding_registers = (uint16*)(this->memory + (register0 - omit));

        this->inregister0 = inregister0;
        this->ninregisters = ninregisters;
        this->input_registers = (uint16*)(this->memory + (inregister0 - omit));
    }
}

ModbusVirtualDevice::~ModbusVirtualDevice() {
    if (this->memory != nullptr) {
        delete this->memory;
    }
}

void ModbusVirtualDevice::initialize_discrete_inputs(uint16 idx, uint8 src) {
    modbus_set_bits_from_byte(this->discrete_inputs, idx, src);
}

void ModbusVirtualDevice::initialize_discrete_inputs(uint16 idx, uint16 size, const uint8* src) {
    modbus_set_bits_from_bytes(this->discrete_inputs, idx, size, src);
}

void ModbusVirtualDevice::initialize_input_registers(uint16 idx, uint16 size, const uint16* src) {
    memcpy(this->input_registers + idx, src, size);
}

/*************************************************************************************************/
int ModbusVirtualDevice::read_coils(uint16 address, uint16 quantity, uint8* coil_status) { // MAP: Page 10
    uint16 idx = address - this->bit0;

    if ((idx < 0) || (idx > this->nbits - quantity)) {
        return -modbus_illegal_address(address, this->bit0, this->nbits, this->debug);
    } else {
        return modbus_read_coils(this->coils, idx, quantity, coil_status, 0);
    }
}

int ModbusVirtualDevice::write_coil(uint16 address, bool value) { // MAP: Page 10
    uint16 idx = address - this->bit0;

    if ((idx < 0) || (idx >= this->nbits)) {
        return -modbus_illegal_address(address, this->bit0, this->nbits, this->debug);
    } else {
        this->coils[idx] = (value ? 1 : 0);
        return 0;
    }
}

int ModbusVirtualDevice::write_coils(uint16 address, uint16 quantity, uint8* src, uint16 count) { // MAP: Page 10
    uint16 idx = address - this->bit0;

    if ((idx < 0) || (idx > this->nbits - quantity)) {
        return -modbus_illegal_address(address, this->bit0, this->nbits, this->debug);
    } else {
        modbus_set_bits_from_bytes(this->coils, idx, count, src);
        return 0;
    }
}
