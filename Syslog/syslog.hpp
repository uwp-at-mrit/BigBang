#pragma once

#include "syslog/logging.hpp"
#include "syslog/receiver/rsyslog.hpp"
#include "syslog/receiver/stdout.hpp"

void rsyslog(Platform::String^ message);
void rsyslog(const wchar_t *fmt, ...);
