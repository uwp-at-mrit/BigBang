#include <algorithm>

#include "string.hpp"

static const wchar_t linefeed = (wchar_t)(0x0A);
static const wchar_t carriage_return = (wchar_t)(0x0D);

static inline size_t integer_length(unsigned int n) {
	return (size_t)(std::floor(log(n) / log(2)) + 1.0);
}

static unsigned int newline_position(const wchar_t* src, unsigned int idx0, unsigned int idxn, unsigned int* next_idx) {
	unsigned int line_size = 0;
	unsigned int eol_size = 0;

	for (unsigned int idx = idx0; idx < idxn; idx ++) {
		if (src[idx] == linefeed) {
			eol_size = (((idx + 1) < idxn) && (src[idx + 1] == carriage_return)) ? 2 : 1;
			break;
		} else if (src[idx] == carriage_return) {
			eol_size = (((idx + 1) < idxn) && (src[idx + 1] == linefeed)) ? 2 : 1;
			break;
		}

		line_size ++;
	}

	(*next_idx) = idx0 + line_size + eol_size;

	return line_size;
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
Platform::String^ substring(Platform::String^ src, int start, int endplus1) {
	Platform::String^ substr = nullptr;
	int max_size = src->Length();
	const wchar_t* pool = (src->Data() + start);
	int subsize = ((endplus1 > 0) ? std::min(endplus1, max_size) : src->Length()) - start;

	if (subsize > 0) {
		substr = ref new Platform::String(pool, subsize);
	}

	return substr;
}

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

/**************************************************************************************************/
Platform::String^ string_first_line(Platform::String^ src) {
	const wchar_t* wsrc = src->Data();
	unsigned int total = src->Length();
	unsigned int line_size = newline_position(wsrc, 0, total, &total);
	
	return ref new Platform::String(wsrc, line_size);
}

std::list<Platform::String^> string_lines(Platform::String^ src, bool skip_empty_line) {
	std::list<Platform::String^> lines;
	unsigned int nidx = 0;
	unsigned int total = src->Length();
	const wchar_t* wsrc = src->Data();

	while (total > 0) {
		unsigned int line_size = newline_position(wsrc, 0, total, &nidx);

		if ((line_size > 0) || (!skip_empty_line)) {
			lines.push_back(ref new Platform::String(wsrc, line_size));
		}

		wsrc += nidx;
		total -= nidx;
	}

	return lines;
}
