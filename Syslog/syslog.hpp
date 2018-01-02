#pragma once

#include "syslog/logging.hpp"
#include "syslog/receiver/rsyslog.hpp"
#include "syslog/receiver/stdout.hpp"

void syslog(WarGrey::SCADA::Log level, Platform::String^ message);
void syslog(WarGrey::SCADA::Log level, const wchar_t *fmt, ...);
