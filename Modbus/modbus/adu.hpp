#pragma once

namespace WarGrey::SCADA {
	void modbus_discard_current_adu();
	void modbus_discard_current_adu(Platform::String^ message, bool debug = true);
	void modbus_discard_current_adu(const wchar_t *fmt, ...);
	void modbus_discard_current_adu(bool debug, const wchar_t *fmt, ...);

	private class modbus_adu_discarded : public std::exception {
	public:
		explicit modbus_adu_discarded(const char * _Message) noexcept : exception(_Message) {}
		modbus_adu_discarded() noexcept : exception() {}
	};
}
