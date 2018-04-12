#include "string.hpp"

using namespace Windows::Foundation;

Platform::String^ filename_from_path(Uri^ uri) {
	return filename_from_path(uri->Path);
}

Platform::String^ filename_from_path(Platform::String^ path) {
	int last_slash_idx = -1;
	int size = path->Length();
	const wchar_t* raw = path->Data();
	Platform::String^ filename = path;

	for (int idx = 0; idx < size; idx++) {
		if (raw[idx] == L'/') {
			last_slash_idx = idx;
		}
	}

	if (last_slash_idx >= 0) {
		wchar_t* fname = new wchar_t[size - last_slash_idx];
		
		for (int idx = last_slash_idx + 1; idx <= size; idx++) {
			fname[idx - last_slash_idx - 1] = raw[idx];
		}

		filename = ref new Platform::String(fname);

		delete[] fname;
	}

	return filename;
}
