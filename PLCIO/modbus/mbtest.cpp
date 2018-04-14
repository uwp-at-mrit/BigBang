#include "modbus/mbtest.hpp"
#include "syslog.hpp"

using namespace WarGrey::SCADA;

static const uint16 ninbit      = 0x16;

static uint8  inbits_src[]      = { 0xAC,  0xDB,  0x35 };
static uint16 registers_src[]   = { 0x22B, 0x001, 0x064 };
static uint16 inregisters_src[] = { 0x0A };

static IModbusServer* server = nullptr;
static IModbusClient* client = nullptr;

class BConfirmation : public ModbusConfirmation {
public:
	void on_discrete_inputs(uint16 transaction, uint16 address, uint8* status, uint8 count, Syslog* logger) override {
		logger->log_message(Log::Info, L"Job(%hu) done, read %hhu input status(0x%02X, 0x%02X, 0x%02X)",
			transaction, count, status[0], status[1], status[2]);
	};

	void on_holding_registers(uint16 transaction, uint16 address, uint16* register_values, uint8 count, Syslog* logger) override {
		logger->log_message(Log::Info, L"Job(%hu) done, read %hhu registers(0x%04X, 0x%04X, 0x%04X)",
			transaction, count, register_values[0], register_values[1], register_values[2]);
	}

	void on_input_registers(uint16 transaction, uint16 address, uint16* register_values, uint8 count, Syslog* logger) override {
		modbus_discard_current_adu(logger,
			L"<Discarded job(%hu) with %hhu input register(0x%04X)>",
			transaction, count, register_values[0]);
	}

public:
	void on_echo_response(uint16 transaction, uint8 function_code, uint16 address, uint16 value, Syslog* logger) override {
		logger->log_message(Log::Info, L"Job(%hu, 0x%02X, 0x%04X, 0x%04X) done", transaction, function_code, address, value);
	};

	void on_exception(uint16 transaction, uint8 function_code, uint16 address0, uint8 reason, Syslog* logger) override {
		logger->log_message(Log::Info, L"Job(%hu, 0x%02X) failed due to reason %d", transaction, function_code, reason);
	};

public:
	void on_private_response(uint16 transaction, uint8 function_code, uint8* data, uint8 count, Syslog* logger) override {
		logger->log_message(Log::Info, L"Job(%hu, 0x%02X) done, surprisingly", transaction, function_code);
	}
};

IModbusServer* make_modbus_test_server(Syslog* logger) {
	uint8 ninregister = sizeof(inregisters_src) / sizeof(uint16);
	auto device = new ModbusVirtualDevice(logger, 0x130, 0x25, 0x1C4, ninbit, 0x160, 0x20, 0x108, ninregister);
	
	device->initialize_discrete_inputs(0, ninbit, inbits_src);
	device->initialize_input_registers(0, ninregister, inregisters_src);

	return device;
}

IModbusClient* make_modbus_test_client(Syslog* logger, Platform::String^ device, IModbusConfirmation* confirmation) {
	return new ModbusClient(logger, device, confirmation);
}

void modbus_test_client(Platform::String^ device, Syslog* logger) {
	if (client == nullptr) {
		auto confirmation = new BConfirmation();
		
		client = make_modbus_test_client(logger, device, confirmation);
	}

	uint8 coils[] = { 0xCD, 0x6B, 0xB2, 0x0E, 0x1B };
	uint16 registers[] = { 0x00, 0x00, 0x00 };
	uint8 regsize = sizeof(registers_src) / sizeof(uint16);
	
	client->write_coil(0x0130, true);
	client->write_coils(0x0130, 0x25, coils);
	client->read_discrete_inputs(0x1C4, 0x16);
	client->read_input_registers(0x108, 0x01);
	client->write_registers(0x160, 0x03, registers_src);
	client->write_read_registers(0x160 + 1, regsize - 1, 0x160, regsize, registers);
}

void modbus_test_server(Syslog* logger) {
	if (server == nullptr) {
		server = make_modbus_test_server(logger);
		server->listen();
	}
	
	modbus_test_client("localhost", logger);
}
