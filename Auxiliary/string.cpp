#include <algorithm>

#include "string.hpp"
#include "syslog.hpp"

static inline size_t integer_length(unsigned int n) {
	return (size_t)(std::floor(log(n) / log(2)) + 1.0);
}

/*************************************************************************************************/
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

std::string binumber(unsigned int n, size_t bitsize) {
	static char spool[64];
	size_t size = ((bitsize < 1) ? ((n == 0) ? 1 : integer_length(n)) : bitsize);
	char* pool = ((size > (sizeof(spool) / sizeof(char))) ? new char[size] : spool);

	for (size_t idx = size; idx > 0; idx--) {
		pool[idx - 1] = (((n >> (size - idx)) & 0b1) ? '1' : '0');
	}

	std::string str(pool, size);

	if (pool != spool) {
		delete[] pool;
	}

	return str;
}
