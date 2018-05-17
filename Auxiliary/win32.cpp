#include "win32.hpp"

#include "path.hpp"

using namespace Windows::Foundation;

HMODULE win32_load_package_library(Platform::String^ dllname, Platform::String^ rootdir) {
	Platform::String^ bslash = L"\\";
	Platform::String^ dll = "usr" + bslash + "share" + bslash + "lib" + bslash + dllname;

	return LoadPackagedLibrary(dll->Data(), 0);
}

Platform::String^ win32_last_strerror() {
	Platform::String^ message = nullptr;
	DWORD flags = FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS;
	DWORD lang = MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT);
	wchar_t* pool;
	DWORD retcode;

	retcode = FormatMessageW(flags, nullptr, GetLastError(), lang, (LPTSTR)&pool, 0, nullptr);

	if (retcode > 0) {
		message = ref new Platform::String(pool);
	}

	LocalFree(pool);

	return message;
}
