#include <ppltasks.h>

#include "modbus/adu.hpp"
#include "rsyslog.hpp"

using namespace WarGrey::SCADA;

void WarGrey::SCADA::modbus_discard_current_adu() {
	throw modbus_adu_discarded();
}

void WarGrey::SCADA::modbus_discard_current_adu(Platform::String^ message, bool debug) {
	if (debug) {
		rsyslog(message);
	}

	modbus_discard_current_adu();
}

void WarGrey::SCADA::modbus_discard_current_adu(const wchar_t *fmt, ...) {
	VSWPRINT(pool, 2048, fmt);
	modbus_discard_current_adu(ref new Platform::String(pool), true);
}

void WarGrey::SCADA::modbus_discard_current_adu(bool debug, const wchar_t *fmt, ...) {
	VSWPRINT(pool, 2048, fmt);
	modbus_discard_current_adu(ref new Platform::String(pool), debug);
}
