#include "string.hpp"
#include "path.hpp"

using namespace Windows::Foundation;

Platform::String^ file_name_from_path(Uri^ uri) {
	return file_name_from_path(uri->Path);
}

Platform::String^ file_name_from_path(Platform::String^ path) {
	int last_slash_idx = -1;
	int size = path->Length();
	const wchar_t* raw = path->Data();
	Platform::String^ filename = path;

	for (int idx = size - 1; idx >= 0; idx--) {
		if (raw[idx] == L'/') {
			last_slash_idx = idx;
			break;
		}
	}

	if (last_slash_idx >= 0) { // TODO: how to deal with directories?
		int fnsize = size - last_slash_idx;
		wchar_t* fname = new wchar_t[fnsize];

		wcscpy_s(fname, fnsize, raw + last_slash_idx + 1);
		filename = ref new Platform::String(fname);

		delete[] fname;
	}

	return filename;
}

Platform::String^ file_extension_from_path(Uri^ uri) {
	return file_extension_from_path(uri->Path);
}

Platform::String^ file_extension_from_path(Platform::String^ path) {
	int last_dot_idx = -1;
	int size = path->Length();
	const wchar_t* raw = path->Data();
	Platform::String^ ext = nullptr;

	for (int idx = size - 1; idx > 0; idx--) { // NOTE: do not count the leading "." for dot files
		if (raw[idx] == L'.') {
			last_dot_idx = idx;
			break;
		}
	}

	if (last_dot_idx >= 0) {
		wchar_t extension[64];

		wcscpy_s(extension, raw + last_dot_idx);
		ext = ref new Platform::String(extension);
	}

	return ext;
}

Uri^ ms_appx_path(Platform::String^ file, Platform::String^ rootdir, Platform::String^ ext) {
	Platform::String^ file_ext = (file_extension_from_path(file) == nullptr) ? (file + ext) : file;
	Platform::String^ path_ext = ((rootdir == nullptr) ? file_ext : (rootdir + "/" + file_ext));

	return ref new Uri("ms-appx:///usr/share/" + path_ext);
}
