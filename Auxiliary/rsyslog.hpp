#pragma once

#include <cwchar>
#include <cstdarg>

#define VSWPRINT(pool, size, fmt) \
static wchar_t pool[size]; \
va_list argl; \
va_start(argl, fmt); \
vswprintf(pool, size, fmt, argl); \
va_end(argl);

void rsyslog(Platform::String^ message);
void rsyslog(const wchar_t *fmt, ...);
