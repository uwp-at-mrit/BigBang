#include "modbus_test.hpp"
#include "rsyslog.hpp"

using namespace WarGrey::SCADA;

static const uint16 ninbit      = 0x16;
static const uint16 ninregister = 0x01;

static uint8  inbits_src[]      = { 0xAC, 0xDB, 0x35 };
static uint16 inregisters_src[] = { 0x0A };

class BConfirmation : public WarGrey::SCADA::IModbusConfirmation {
public:
	void on_discrete_inputs(uint16 transaction, uint8* status, uint8 count) override {
		rsyslog(L"Job(%hu) done, read %hhu input status(%02X, %02X, %02X)",
			transaction, count, status[0], status[1], status[2]);
	};

	void on_echo_response(uint16 transaction, uint8 function_code, uint16 address, uint16 value) override {
		rsyslog(L"Job(%hu, 0x%02X, 0x%04X, 0x%04X) done", transaction, function_code, address, value);
	};

	void on_exception(uint16 transaction, uint8 function_code, uint8 reason) override {
		rsyslog(L"Job(%hu, 0x%02X) failed due to reason %d", transaction, function_code, reason);
	};
};

IModbusServer* make_modbus_test_server() {
	auto device = new ModbusVirtualDevice(0x130, 0x25, 0x1C4, ninbit, 0x160, 0x20, 0x108, ninregister);
	
	device->initialize_discrete_inputs(0, ninbit, inbits_src);
	device->initialize_input_registers(0, ninregister, inregisters_src);
	device->enable_debug(true);

	return device;
}

IModbusClient* make_modbus_test_client(Platform::String^ device) {
	auto client = new ModbusClient(device);

	client->enable_debug(true);

	return client;
}

void modbus_test_client(Platform::String^ device) {
	uint8 coils[] = { 0xCD, 0x6B, 0xB2, 0x0E, 0x1B };
	auto confirmation = new BConfirmation();
	auto client = make_modbus_test_client(device);
	
	client->write_coil(0x0130, true, confirmation);
	client->write_coils(0x0130, 0x25, coils, confirmation);
	client->read_discrete_inputs(0x1C4, 0x16, confirmation);
}
