#include "string.hpp"
#include "time.hpp"
#include "path.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::Storage;

static int last_slash_position(const wchar_t* raw, int size, int fallback = -1) {
	int index = fallback;

	for (int idx = size - 1; idx >= 0; idx--) {
		if ((raw[idx] == L'/') || (raw[idx] == L'\\')) {
			index = idx;
			break;
		}
	}

	return index;
}

static int last_dot_position(const wchar_t* raw, int size, int fallback = -1) {
	int index = fallback;

	for (int idx = size - 1; idx > 0; idx--) { // NOTE: do not count the leading "." for dot files
		if (raw[idx] == L'.') {
			index = idx;
			break;
		}
	}

	return index;
}

/*************************************************************************************************/
Platform::String^ WarGrey::SCADA::path_only(Uri^ uri) {
	return path_only(uri->Path);
}

Platform::String^ WarGrey::SCADA::path_only(Platform::String^ path) {
	Platform::String^ dirname = "";
	unsigned int size = path->Length();
	const wchar_t* raw = path->Data();
	int last_slash_idx = last_slash_position(raw, size);

	if (last_slash_idx >= 0) {
		dirname = substring(path, 0, last_slash_idx + 1);
	}

	return dirname;
}

Platform::String^ WarGrey::SCADA::file_name_from_path(Uri^ uri) {
	return file_name_from_path(uri->Path);
}

Platform::String^ WarGrey::SCADA::file_name_from_path(Platform::String^ path) {
	Platform::String^ filename = path;
	unsigned int size = path->Length();
	const wchar_t* raw = path->Data();
	int last_slash_idx = last_slash_position(raw, size);
	
	if (last_slash_idx >= 0) { // TODO: how to deal with directories?
		filename = substring(path, last_slash_idx + 1, size);
	}

	return filename;
}

Platform::String^ WarGrey::SCADA::file_basename_from_path(Uri^ uri) {
	return file_basename_from_path(uri->Path);
}

Platform::String^ WarGrey::SCADA::file_basename_from_path(Platform::String^ path) {
	unsigned int size = path->Length();
	const wchar_t* raw = path->Data();
	int last_dot_idx = last_dot_position(raw, size, size);
	int last_slash_idx = last_slash_position(raw, last_dot_idx, -1);
	
	return substring(path, last_slash_idx + 1, last_dot_idx);
}

Platform::String^ WarGrey::SCADA::file_extension_from_path(Uri^ uri) {
	return file_extension_from_path(uri->Path);
}

Platform::String^ WarGrey::SCADA::file_extension_from_path(Platform::String^ path) {
	Platform::String^ ext = nullptr;
	unsigned int size = path->Length();
	const wchar_t* raw = path->Data();
	int last_dot_idx = last_dot_position(raw, size);
	
	if (last_dot_idx >= 0) {
		ext = substring(path, last_dot_idx, size);
	}

	return ext;
}

Platform::String^ WarGrey::SCADA::file_basename_from_second(long long timepoint, bool locale) {
	long long hour, minute, seconds;

	// Stupid Windows Machine, ":" cannot be included in filename

	split_time_utc(timepoint, locale, &hour, &minute, &seconds);

	return make_datestamp_utc(timepoint, locale) + "T" + make_wstring(L"%02d_%02d_%02d", hour, minute, seconds);
}

Uri^ WarGrey::SCADA::ms_appx_file(Platform::String^ file, Platform::String^ ext, Platform::String^ rootdir) {
	Platform::String^ file_ext = (file_extension_from_path(file) == nullptr) ? (file + ext) : file;
	Platform::String^ path_ext = ((rootdir == nullptr) ? file_ext : (rootdir + "/" + file_ext));

	return ref new Uri("ms-appx:///stone/" + path_ext);
}

Platform::String^ WarGrey::SCADA::ms_apptemp_file(Platform::String^ file, Platform::String^ ext) {
	StorageFolder^ apptemp = ApplicationData::Current->TemporaryFolder;
	Platform::String^ file_ext = (file_extension_from_path(file) == nullptr) ? (file + ext) : file;

	return apptemp->Path + "\\" + file_ext;
}
