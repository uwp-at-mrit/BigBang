#include "syslog.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

ISyslogReceiver* default_racket_receiver() {
	static ISyslogReceiver* rsyslog;

	if (rsyslog == nullptr) {
		rsyslog = new RacketReceiver("172.16.1.1", 18030, Log::Debug);
		rsyslog->reference();
	}

	return rsyslog;
}

Syslog* default_logger() {
	static Syslog* winlog;

	if (winlog == nullptr) {
		winlog = new Syslog(Log::Debug, "WinSCADA", nullptr);
		winlog->append_log_receiver(default_racket_receiver());
		winlog->append_log_receiver(new StdoutReceiver());
		winlog->reference();
	}

	return winlog;
}

void syslog(Log level, Platform::String^ message) {
	auto self = default_logger();

	self->log_message(level, message);
}

void syslog(Log level, const wchar_t *fmt, ...) {
	VSWPRINT(message, fmt);

	syslog(level, message);
}

/*************************************************************************************************/
#define implement_syslog(fname, level) \
void syslog_##fname(const wchar_t *fmt, ...) { VSWPRINT(message, fmt); syslog(level, message); } \
void syslog_##fname(Platform::String^ message) { syslog(level, message); }

implement_syslog(debug,    Log::Debug)
implement_syslog(info,     Log::Info)
implement_syslog(notice,   Log::Notice)
implement_syslog(warning,  Log::Warning)
implement_syslog(error,    Log::Error)
implement_syslog(critical, Log::Critical)
implement_syslog(alert,    Log::Alert)
implement_syslog(panic,    Log::Panic)

#undef implement_syslog
