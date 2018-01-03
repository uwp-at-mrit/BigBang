#pragma once

#include <cinttypes>
#include <exception>

#include "sockexn.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	uint8 modbus_illegal_function(uint8 function_code, WarGrey::SCADA::Syslog* logger);
	uint8 modbus_illegal_address(uint16 address, uint16 start, uint16 amount, WarGrey::SCADA::Syslog* logger);
	uint8 modbus_illegal_address(uint16 address, uint16 count, uint16 start, uint16 amount, WarGrey::SCADA::Syslog* logger);

	uint8 modbus_identification_not_found(uint16 id, WarGrey::SCADA::Syslog* logger);
	uint8 modbus_identification_not_found(uint16 id, uint8 start, uint8 end, WarGrey::SCADA::Syslog* logger);
	uint8 modbus_identification_not_found(uint16 id, uint8 start, uint8 end, uint8 product, WarGrey::SCADA::Syslog* logger);

	uint8 modbus_illegal_data_value(uint16 value, uint16 vexpected, WarGrey::SCADA::Syslog* logger);
	uint8 modbus_illegal_data_value(uint16 value, uint16 vmin, uint16 vmax, WarGrey::SCADA::Syslog* logger);
	uint8 modbus_illegal_enum_value(uint16 value, uint16 v1, uint16 v2, WarGrey::SCADA::Syslog* logger);

	void modbus_protocol_fatal();
	void modbus_protocol_fatal(WarGrey::SCADA::Syslog* logger, Platform::String^ message);
	void modbus_protocol_fatal(WarGrey::SCADA::Syslog* logger, const wchar_t *fmt, ...);

	void modbus_discard_current_adu();
	void modbus_discard_current_adu(Platform::String^ message, WarGrey::SCADA::Syslog* logger);
	void modbus_discard_current_adu(WarGrey::SCADA::Syslog* logger, const wchar_t *fmt, ...);

	private class modbus_error : public std::exception {
	public:
		modbus_error() noexcept : exception() {}
	};

	private class modbus_discarded : public std::exception {
	public:
		modbus_discarded() noexcept : exception() {}
	};
}
