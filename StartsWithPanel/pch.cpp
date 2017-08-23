#include "pch.h"

// WARNING: this kind of file is important for Visual Studio Solution,
//            or it will complain the missing of precompiled header files.
//            See project properties -> C/C++ -> Precompiled Headers

void trace(String^ message) {
    OutputDebugString(message->Data());
    OutputDebugString(L"\n");
}
