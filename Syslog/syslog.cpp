﻿#include "syslog.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

static Syslog* winlog;

void syslog(Platform::String^ message) {
	if (winlog == nullptr) {
		winlog = new Syslog(Log::Debug, "WinSCADA", nullptr);
		winlog->append_log_receiver(new StdoutReceiver());
		winlog->append_log_receiver(new RSyslogReceiver("172.16.1.1", 18030));
	}

	winlog->log_message(Log::Debug, message, "");
}

void syslog(const wchar_t *fmt, ...) {
	VSWPRINT(message, fmt);

    syslog(message);
}
