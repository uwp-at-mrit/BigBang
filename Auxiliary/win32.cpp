#include "win32.hpp"

#include "path.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

HMODULE win32_load_foreign_library(Platform::String^ dllname, Syslog* logger) {
	HMODULE m = nullptr;
	Platform::String^ bslash = L"\\";
	Platform::String^ dll = "stone" + bslash + "lib" + bslash + dllname;

	m = LoadPackagedLibrary(dll->Data(), 0);

	if (m == nullptr) {
		if (logger != nullptr) {
			logger->log_message(Log::Error,
				L"unable to load library %s: %s",
				dll->Data(),
				win32_last_strerror()->Data());
		}
	}

	return m;
}

bool win32_unload_foreign_library(HMODULE m, Syslog* logger) {
	int okay = FreeLibrary(m);

	if (okay == 0) {
		if (logger != nullptr) {
			logger->log_message(Log::Error,
				L"unable to unload library: %s",
				win32_last_strerror()->Data());
		}
	}

	return (okay > 0);
}

FARPROC win32_fetch_foreign_object(HMODULE m, LPCSTR name, Syslog* logger) {
	FARPROC fobj = GetProcAddress(m, name);

	if (fobj == nullptr) {
		if (logger != nullptr) {
			size_t size = strlen(name);
			Platform::String^ wname = make_wstring(name);

			logger->log_message(Log::Error,
				L"unable to fetch object '%s': %s",
				wname->Data(),
				win32_last_strerror()->Data());
		}
	}

	return fobj;
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
