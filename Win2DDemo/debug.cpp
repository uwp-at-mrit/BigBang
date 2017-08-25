#include "debug.h"

#include <ppltasks.h>

void trace(Platform::String^ message) {
    OutputDebugString(message->Data());
    OutputDebugString(L"\n");
}
