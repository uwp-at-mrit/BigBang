#include "syslog.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

ISyslogReceiver* default_rsyslog_receiver() {
	static ISyslogReceiver* rsyslog;

	if (rsyslog == nullptr) {
		rsyslog = new RacketReceiver("172.16.1.1", 18030);
		rsyslog->reference();
	}

	return rsyslog;
}

Syslog* default_logger() {
	static Syslog* winlog;

	if (winlog == nullptr) {
		winlog = new Syslog(Log::Debug, "WinSCADA", nullptr);
		winlog->append_log_receiver(default_rsyslog_receiver());
		winlog->append_log_receiver(new StdoutReceiver());
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
