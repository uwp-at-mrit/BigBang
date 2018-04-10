#include "modbus/exception.hpp"
#include "modbus/codes.hpp"

#include "string.hpp"
#include "syslog.hpp"

using namespace WarGrey::SCADA;

uint8 WarGrey::SCADA::modbus_illegal_function(uint8 function_code, Syslog* logger) {
	logger->log_message(Log::Debug, L"[Unknown function code 0x%02X]", function_code);

	return MODBUS_EXN_ILLEGAL_FUNCTION;
}

uint8 WarGrey::SCADA::modbus_illegal_address(uint16 address, uint16 start, uint16 amount, Syslog* logger) {
	logger->log_message(Log::Debug, L"[Illegal data address 0x%04X, out of range [0x%04X, 0x%04X)]", address, start, start + amount);

	return MODBUS_EXN_ILLEGAL_DATA_ADDRESS;
}

uint8 WarGrey::SCADA::modbus_illegal_address(uint16 address, uint16 count, uint16 start, uint16 amount, Syslog* logger) {
	if ((address < start) || (address > start + amount)) {
		modbus_illegal_address(address, start, amount, logger);
	} else {
		logger->log_message(Log::Debug, L"[Too many data required (%u > %u)]", address - start + count, amount);
	}

	return MODBUS_EXN_ILLEGAL_DATA_ADDRESS;
}

uint8 WarGrey::SCADA::modbus_identification_not_found(uint16 id, Syslog* logger) {
	logger->log_message(Log::Debug, L"[No such device identification 0x%02X", id);

	return MODBUS_EXN_ILLEGAL_DATA_ADDRESS;
}

uint8 WarGrey::SCADA::modbus_identification_not_found(uint16 id, uint8 start, uint8 end, Syslog* logger) {
	logger->log_message(Log::Debug, L"[No such device identification 0x%02X, please search in range [0x%02X, 0x%02X]]", id, start, end);

	return MODBUS_EXN_ILLEGAL_DATA_ADDRESS;
}

uint8 WarGrey::SCADA::modbus_identification_not_found(uint16 id, uint8 start, uint8 end, uint8 product, Syslog* logger) {
	logger->log_message(Log::Debug, L"[No such device identification 0x%02X, please search in range [0x%02X, 0x%02X] or [0x%02X, 0x%02X]]",
		id, start, end, product, 0xFF);

	return MODBUS_EXN_ILLEGAL_DATA_ADDRESS;
}

uint8 WarGrey::SCADA::modbus_illegal_data_value(uint16 value, uint16 vexpected, Syslog* logger) {
	logger->log_message(Log::Debug, L"[Illegal data value, expected 0x%04X, given 0x%04X]", vexpected, value);

	return MODBUS_EXN_ILLEGAL_DATA_VALUE;
}

uint8 WarGrey::SCADA::modbus_illegal_data_value(uint16 value, uint16 vmin, uint16 vmax, Syslog* logger) {
	logger->log_message(Log::Debug, L"[Illegal data value 0x%04X, out of range [0x%04X, 0x%04X)]", value, vmin, vmax);

	return MODBUS_EXN_ILLEGAL_DATA_VALUE;
}

uint8 WarGrey::SCADA::modbus_illegal_enum_value(uint16 value, uint16 v1, uint16 v2, Syslog* logger) {
	logger->log_message(Log::Debug, L"[Illegal data value 0x%04X, out of range {0x%04X, 0x%04X}]", value, v1, v2);

	return MODBUS_EXN_ILLEGAL_DATA_VALUE;
}

/*************************************************************************************************/
void WarGrey::SCADA::modbus_protocol_fatal() {
	throw task_terminated();
}

void WarGrey::SCADA::modbus_protocol_fatal(Syslog* logger, Platform::String^ message) {
	logger->log_message(Log::Critical, message);
	modbus_protocol_fatal();
}

void WarGrey::SCADA::modbus_protocol_fatal(Syslog* logger, const wchar_t *fmt, ...) {
	VSNWPRINT(pool, 2048, fmt);
	modbus_protocol_fatal(logger, ref new Platform::String(pool));
}

/*************************************************************************************************/
void WarGrey::SCADA::modbus_discard_current_adu() {
	throw task_discarded();
}

void WarGrey::SCADA::modbus_discard_current_adu(Platform::String^ message, Syslog* logger) {
	logger->log_message(Log::Error, message);

	modbus_discard_current_adu();
}

void WarGrey::SCADA::modbus_discard_current_adu(Syslog* logger, const wchar_t *fmt, ...) {
	VSNWPRINT(pool, 2048, fmt);
	modbus_discard_current_adu(ref new Platform::String(pool), logger);
}
