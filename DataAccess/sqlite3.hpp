#pragma once

#include "dbsystem.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	typedef void* sqlite3_t;

	private class SQLite3 : public WarGrey::SCADA::DBSystem {
	public:
		virtual ~SQLite3() noexcept;

		SQLite3(const wchar_t* dbfile = L"", WarGrey::SCADA::Syslog* logger = nullptr);

	public:
		Platform::String^ get_name() override;
		int get_version();

	private:
		WarGrey::SCADA::sqlite3_t master;
	};
}
