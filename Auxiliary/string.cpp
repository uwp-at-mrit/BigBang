#include "string.hpp"

Platform::String^ make_string(const wchar_t* fmt, ...) {
	VSWPRINT(s, fmt);
	
	return s;
}