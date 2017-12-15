#pragma once

#include <cinttypes>
#include <exception>

#include "sockexn.hpp"

namespace WarGrey::SCADA {
	uint8 modbus_illegal_function(uint8 function_code, bool debug = true);
	uint8 modbus_illegal_address(uint16 address, uint16 start, uint16 amount, bool debug = true);
	uint8 modbus_illegal_address(uint16 address, uint16 count, uint16 start, uint16 amount, bool debug = true);

	uint8 modbus_identification_not_found(uint16 id, bool debug = true);
	uint8 modbus_identification_not_found(uint16 id, uint8 start, uint8 end, bool debug = true);
	uint8 modbus_identification_not_found(uint16 id, uint8 start, uint8 end, uint8 product, bool debug = true);

	uint8 modbus_illegal_data_value(uint16 value, uint16 vexpected, bool debug = true);
	uint8 modbus_illegal_data_value(uint16 value, uint16 vmin, uint16 vmax, bool debug = true);
	uint8 modbus_illegal_enum_value(uint16 value, uint16 v1, uint16 v2, bool debug = true);

	void modbus_protocol_fatal();
	void modbus_protocol_fatal(Platform::String^ message);
	void modbus_protocol_fatal(const wchar_t *fmt, ...);

	void modbus_discard_current_adu();
	void modbus_discard_current_adu(Platform::String^ message, bool debug = true);
	void modbus_discard_current_adu(const wchar_t *fmt, ...);
	void modbus_discard_current_adu(bool debug, const wchar_t *fmt, ...);

	private class modbus_error : public std::exception {
	public:
		modbus_error() noexcept : exception() {}
	};

	private class modbus_discarded : public std::exception {
	public:
		modbus_discarded() noexcept : exception() {}
	};
}
