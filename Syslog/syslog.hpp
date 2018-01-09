#pragma once

#include "syslog/logging.hpp"
#include "syslog/receiver/racket.hpp"
#include "syslog/receiver/stdout.hpp"

WarGrey::SCADA::Syslog* default_logger();
WarGrey::SCADA::ISyslogReceiver* default_rsyslog_receiver();

void syslog(WarGrey::SCADA::Log level, Platform::String^ message);
void syslog(WarGrey::SCADA::Log level, const wchar_t *fmt, ...);
