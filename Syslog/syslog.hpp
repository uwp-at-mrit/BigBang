#pragma once

#include "syslog/logging.hpp"
#include "syslog/receiver/racket.hpp"
#include "syslog/receiver/vstudio.hpp"

void set_default_logging_level(WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Debug);
void set_default_logging_topic(Platform::String^ topic = "WinSCADA");
void set_default_racket_receiver_host(Platform::String^ ipv4);

WarGrey::SCADA::Syslog* default_logger();
WarGrey::SCADA::Syslog* make_silent_logger(Platform::String^ topic);
WarGrey::SCADA::Syslog* make_system_logger(Platform::String^ topic);
WarGrey::SCADA::Syslog* make_system_logger(WarGrey::SCADA::Log level, Platform::String^ topic);
WarGrey::SCADA::Syslog* make_logger(WarGrey::SCADA::Log level, Platform::String^ topic, WarGrey::SCADA::Syslog* parent = nullptr);

WarGrey::SCADA::RacketReceiver* default_racket_receiver();

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
