#pragma once

#include <Windows.h>

HMODULE win32_load_package_library(Platform::String^ dllname);

Platform::String^ win32_last_strerror();
