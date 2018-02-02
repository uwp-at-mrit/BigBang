#pragma once

#include "syslog/logging.hpp"
#include "syslog/receiver/racket.hpp"
#include "syslog/receiver/vstudio.hpp"

WarGrey::SCADA::Syslog* default_logger();
WarGrey::SCADA::ISyslogReceiver* default_racket_receiver();

void syslog(WarGrey::SCADA::Log level, Platform::String^ message);
void syslog(WarGrey::SCADA::Log level, const wchar_t *fmt, ...);

#define declare_syslog(level) \
    void syslog_##level(const wchar_t *fmt, ...); \
    void syslog_##level(Platform::String^ message);

declare_syslog(debug)
declare_syslog(info)
declare_syslog(notice)
declare_syslog(warning)
declare_syslog(error)
declare_syslog(critical)
declare_syslog(alert)
declare_syslog(panic)

#undef declare_syslog
