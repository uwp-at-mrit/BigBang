#pragma once

#include <string>
#include <cwchar>
#include <cstdarg>

#define VSNWPRINT(pool, size, fmt) \
    static wchar_t pool[size]; \
    va_list argl; \
    va_start(argl, fmt); \
    vswprintf(pool, size, fmt, argl); \
    va_end(argl);

#define VSWPRINT(retval, fmt) \
Platform::String^ retval; { \
    static const int DEFAULT_POOL_SIZE = 1024; \
    static wchar_t wpool[DEFAULT_POOL_SIZE]; \
    int bigSize = DEFAULT_POOL_SIZE - 1; \
    wchar_t* pool; \
    va_list argl; \
    do { \
	    pool = (bigSize < DEFAULT_POOL_SIZE) ? wpool : (new wchar_t[bigSize + 1]); \
    	va_start(argl, fmt); \
    	int status = vswprintf(pool, bigSize + 1, fmt, argl); \
	    va_end(argl); \
    	if (status == -1) { \
	    	bigSize = bigSize * 2 + 1; \
		    if (pool != wpool) delete[] pool; \
		    pool = nullptr; \
	    } \
    } while (pool == nullptr); \
    retval = ref new Platform::String(pool); \
    if (pool != wpool) delete[] pool; \
}

#define VSNPRINT(retval, fmt) \
std::string retval; { \
    static const int DEFAULT_POOL_SIZE = 1024; \
    static char chpool[DEFAULT_POOL_SIZE]; \
    int bigSize = DEFAULT_POOL_SIZE - 1; \
    char* pool; \
    va_list argl; \
    do { \
	    pool = (bigSize < DEFAULT_POOL_SIZE) ? chpool : (new char[bigSize + 1]); \
    	va_start(argl, fmt); \
    	int status = vsnprintf(pool, bigSize + 1, fmt, argl); \
	    va_end(argl); \
    	if (status == -1) { \
	    	bigSize = bigSize * 2 + 1; \
		    if (pool != chpool) delete[] pool; \
		    pool = nullptr; \
	    } \
    } while (pool == nullptr); \
    retval = std::string(pool); \
    if (pool != chpool) delete[] pool; \
}

Platform::String^ make_wstring(const wchar_t* fmt, ...);
Platform::String^ make_wstring(const char* bytes);
Platform::String^ make_wstring(std::string bytes);
size_t wstrlen(const wchar_t* content);

std::string make_nstring(const char* fmt, ...);
std::string make_nstring(const wchar_t* wbytes);
std::string make_nstring(Platform::String^ wstr);

std::string binumber(unsigned int n, size_t bitsize = 0);
