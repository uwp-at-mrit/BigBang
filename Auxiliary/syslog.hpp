#pragma once

#include "syslog/logging.hpp"

void rsyslog(Platform::String^ message);
void rsyslog(const wchar_t *fmt, ...);
