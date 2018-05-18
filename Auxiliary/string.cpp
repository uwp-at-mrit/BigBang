#include "string.hpp"

Platform::String^ make_string(const wchar_t* fmt, ...) {
	VSWPRINT(s, fmt);
	
	return s;
}

size_t wstrlen(const wchar_t* content) {
	return int(wcslen(content)) * 2 - 1;
}
