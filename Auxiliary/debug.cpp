#include <ppltasks.h>

#include "debug.hpp"

#define DEFAULT_POOL_SIZE 2048
static wchar_t pool[DEFAULT_POOL_SIZE];

void trace(Platform::String^ message) {
    OutputDebugString(message->Data());
    OutputDebugString(L"\n");
}

void trace(const wchar_t *fmt) {
    OutputDebugString(fmt);
    OutputDebugString(L"\n");
}

void trace(const wchar_t *fmt, ...) {
    va_list argl;
    va_start(argl, fmt);
    vswprintf(pool, DEFAULT_POOL_SIZE, fmt, argl);
    va_end(argl);

    OutputDebugString(pool);
    OutputDebugString(L"\n");
}
