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
int ModbusVirtualDevice::read_coils(uint16 address, uint16 quantity, uint8* coil_status) { // MAP: Page 11
    uint16 idx = address - this->bit0;

    if ((idx < 0) || (idx > this->nbits - quantity)) {
        return -modbus_illegal_address(address, this->bit0, this->nbits, this->debug);
    } else {
        return modbus_read_coils(this->coils, idx, quantity, coil_status);
    }
}

int ModbusVirtualDevice::read_discrete_inputs(uint16 address, uint16 quantity, uint8* input_status) { // MAP: Page 12
	uint16 idx = address - this->inbit0;

	if ((idx < 0) || (idx > this->ninbits - quantity)) {
		return -modbus_illegal_address(address, this->inbit0, this->ninbits, this->debug);
	} else {
		return modbus_read_coils(this->discrete_inputs, idx, quantity, input_status);
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

int ModbusVirtualDevice::write_coils(uint16 address, uint16 quantity, uint8* src) { // MAP: Page 10
    uint16 idx = address - this->bit0;

    if ((idx < 0) || (idx > this->nbits - quantity)) {
        return -modbus_illegal_address(address, this->bit0, this->nbits, this->debug);
    } else {
        modbus_set_bits_from_bytes(this->coils, idx, quantity, src);
        return 0;
    }
}

int ModbusVirtualDevice::read_holding_registers(uint16 address, uint16 quantity, uint8* register_values) { // MAP: Page 15
	uint16 idx = address - this->register0;

	if ((idx < 0) || (idx > this->nregisters - quantity)) {
		return -modbus_illegal_address(address, this->register0, this->nregisters, this->debug);
	} else {
		return modbus_read_registers(this->holding_registers, idx, quantity, register_values);
	}
}

int ModbusVirtualDevice::read_input_registers(uint16 address, uint16 quantity, uint8* input_registers) { // MAP: Page 16
	uint16 idx = address - this->inregister0;

	if ((idx < 0) || (idx > this->ninregisters - quantity)) {
		return -modbus_illegal_address(address, this->inregister0, this->ninregisters, this->debug);
	} else {
		return modbus_read_registers(this->input_registers, idx, quantity, input_registers);
	}
}

int ModbusVirtualDevice::write_register(uint16 address, uint16 value) { // MAP: Page 19
	uint16 idx = address - this->register0;

	if ((idx < 0) || (idx >= this->nregisters)) {
		return -modbus_illegal_address(address, this->register0, this->nregisters, this->debug);
	} else {
		this->holding_registers[idx] = value;
		return 0;
	}
}

int ModbusVirtualDevice::write_registers(uint16 address, uint16 quantity, uint8* src) { // MAP: Page 10
	uint16 idx = address - this->register0;

	if ((idx < 0) || (idx > this->nregisters - quantity)) {
		return -modbus_illegal_address(address, this->register0, this->nregisters, this->debug);
	} else {
		modbus_write_registers(this->holding_registers, idx, quantity, src);
		return 0;
	}
}

int ModbusVirtualDevice::write_read_registers(uint16 waddr, uint16 wcount, uint16 raddr, uint16 rcount, uint8* rwpool) { // MAP: Page 38
	uint16 widx = waddr - this->register0;
	uint16 ridx = raddr - this->register0;

	if ((widx < 0) || (widx > this->nregisters - wcount)) {
		return -modbus_illegal_address(waddr, this->register0, this->nregisters, this->debug);
	} else if ((ridx < 0) || (ridx > this->nregisters - rcount)) {
		return -modbus_illegal_address(raddr, this->register0, this->nregisters, this->debug);
	} else {
		modbus_write_registers(this->holding_registers, widx, wcount, rwpool);
		return modbus_read_registers(this->holding_registers, ridx, rcount, rwpool);
	}
}
