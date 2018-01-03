#include "syslog.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

static Syslog* winlog;

Syslog* default_logger() {
	if (winlog == nullptr) {
		winlog = new Syslog(Log::Debug, "WinSCADA", nullptr);
		winlog->append_log_receiver(new StdoutReceiver());
		winlog->append_log_receiver(new RSyslogReceiver("172.16.1.1", 18030));
		winlog->reference();
	}

	return winlog;
}

void syslog(Log level, Platform::String^ message) {
	auto self = default_logger();

	self->log_message(level, message, "");
}

void syslog(Log level, const wchar_t *fmt, ...) {
	VSWPRINT(message, fmt);

    syslog(level, message);
}
