#include "string.hpp"

/** WARNING
 * https://docs.microsoft.com/en-us/cpp/c-runtime-library/format-specification-syntax-printf-and-wprintf-functions
 *
 * The behavior of the `c`, `C`, `s`, and `S` type characters are Microsoft extensions.
 * The ISO C standard uses `c` and `s` consistently for narrow characters and strings,
 *   and `C` and `S` for wide characters and strings, in all formatting functions.
 */

Platform::String^ make_wstring(const wchar_t* fmt, ...) {
	VSWPRINT(s, fmt);
	
	return s;
}

Platform::String^ make_wstring(const char* bytes) {
	return make_wstring(L"%S", bytes);
}

Platform::String^ make_wstring(std::string bytes) {
	return make_wstring(L"%S", bytes.c_str());
}

size_t wstrlen(const wchar_t* content) {
	return int(wcslen(content)) * 2 - 1;
}

/*************************************************************************************************/
std::string make_nstring(const char* fmt, ...) {
	VSNPRINT(s, fmt);

	return s;
}

std::string make_nstring(const wchar_t* wbytes) {
	return make_nstring("%S", wbytes);
}

std::string make_nstring(Platform::String^ wstr) {
	return make_nstring("%S", wstr->Data());
}
