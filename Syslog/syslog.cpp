#include "syslog.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

static Platform::String^ default_racket_receiver_host = nullptr;
static Platform::String^ default_logger_topic = "WinSCADA";
static Log default_logger_level = Log::Debug;

void set_default_logging_level(Log level) {
	default_logger_level = level;
}

void set_default_logging_topic(Platform::String^ topic) {
	default_logger_topic = topic;
}

void set_default_racket_receiver_host(Platform::String^ ipv4) {
	default_racket_receiver_host = ipv4;
}

/*************************************************************************************************/
RacketReceiver* default_racket_receiver() {
	static RacketReceiver* rsyslog;

	if ((rsyslog == nullptr) && (default_racket_receiver_host != nullptr)) {
		rsyslog = new RacketReceiver(default_racket_receiver_host, 18030, Log::Debug);
		rsyslog->reference();
	}

	return rsyslog;
}

Syslog* default_logger() {
	static Syslog* winlog;

	if (winlog == nullptr) {
		RacketReceiver* racket = default_racket_receiver();

		winlog = make_logger(default_logger_level, default_logger_topic, nullptr);
		winlog->append_log_receiver(new VisualStudioReceiver());

		if (racket != nullptr) {
			winlog->append_log_receiver(racket);
		}

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
implement_syslog(alert,    Log::Alarm)
implement_syslog(panic,    Log::Panic)

#undef implement_syslog

/*************************************************************************************************/
Syslog* make_logger(Log level, Platform::String^ topic, Syslog* parent) {
	return new Syslog(level, topic, parent);
}

Syslog* make_silent_logger(Platform::String^ topic) {
	return make_logger(Log::None, topic);
}

Syslog* make_system_logger(Log level, Platform::String^ topic) {
	return make_logger(level, topic, default_logger());
}
