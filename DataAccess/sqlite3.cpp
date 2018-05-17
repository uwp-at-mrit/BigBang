#include "sqlite3.hpp"
#include "win32.hpp"

using namespace WarGrey::SCADA;

typedef int(*_fun__wchar__sqlite3__int)(const wchar_t*, sqlite3_t*);
typedef wchar_t* (*_fun__sqlite3__wchar)(sqlite3_t);
typedef int (*_fun__sqlite3__int)(sqlite3_t);

static HMODULE sqlite3 = nullptr;
static int references = 0;

static _fun__int sqlite3_libversion_number;
static _fun__wchar__sqlite3__int sqlite3_open16;
static _fun__sqlite3__int sqlite3_close;
static _fun__sqlite3__wchar sqlite3_errmsg16;

static int SQLITE_OK = 0;

static void load_sqlite3(Syslog* logger) {
	if (sqlite3 == nullptr) {
		sqlite3 = win32_load_foreign_library("sqlite3", logger);
		win32_fetch_foreign_object(sqlite3, "sqlite3_open", logger);

		win32_fetch(sqlite3, sqlite3_libversion_number, _fun__int, logger);
		win32_fetch(sqlite3, sqlite3_open16, _fun__wchar__sqlite3__int, logger);
		win32_fetch(sqlite3, sqlite3_close, _fun__sqlite3__int, logger);
		win32_fetch(sqlite3, sqlite3_errmsg16, _fun__sqlite3__wchar, logger);
	}

	references += 1;
}

static void unload_sqlite3(Syslog* logger) {
	references -= 1;

	if (references <= 0) {
		if (sqlite3 != nullptr) {
			win32_unload_foreign_library(sqlite3, logger);
			sqlite3 = nullptr;
		}
	}
}

/*************************************************************************************************/
SQLite3::SQLite3(const wchar_t* dbfile, Syslog* logger) : DBSystem(logger) {
	const wchar_t* target = ((dbfile == L"") ? L":memory:" : dbfile);

	load_sqlite3(this->get_logger());
	
	if (sqlite3_open16(target, &this->master) != SQLITE_OK) {
		this->get_logger()->log_message(Log::Error, L"%s", sqlite3_errmsg16(this->master));
	}
}

SQLite3::~SQLite3() {
	unload_sqlite3(this->get_logger());

	if (sqlite3_close(this->master) != SQLITE_OK) {
		this->get_logger()->log_message(Log::Warning, L"%s", sqlite3_errmsg16(this->master));
	}
}

Platform::String^ SQLite3::get_name() {
	return "SQLite3";
}

int SQLite3::get_version() {
	return sqlite3_libversion_number();
}
