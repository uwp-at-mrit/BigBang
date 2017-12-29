#pragma once

#include "syslog/logging.hpp"
#include "syslog/receiver/rsyslog.hpp"
#include "syslog/receiver/stdout.hpp"

void syslog(Platform::String^ message);
void syslog(const wchar_t *fmt, ...);
