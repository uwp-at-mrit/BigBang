#pragma once

#include <shared_mutex>

#include "dbsystem.hpp"
#include "sqlite3/dll.hpp"

namespace WarGrey::SCADA {
	private class RotativeSQLite3 : public WarGrey::SCADA::ISQLite3 {
	public:
		virtual ~RotativeSQLite3() noexcept;

		RotativeSQLite3(const wchar_t* dbdir, WarGrey::SCADA::Syslog* logger = nullptr,
			sqlite3_trace_f xCallback = WarGrey::SCADA::sqlite3_default_trace_callback);

	public:
		std::list<std::string> list_tables() override;
		bool table_exists(const std::string& tablename) override;
		std::string get_last_error_message() override;
		WarGrey::SCADA::IPreparedStatement* prepare(const std::string& sql) override;

	public:
		std::list<WarGrey::SCADA::SQliteTableInfo> table_info(const char* name) override;

	public:
		int libversion() override;
		int changes(bool total = false) override;
		int64 last_insert_rowid() override;

	protected:
		WarGrey::SCADA::IVirtualSQL* new_sql_factory(WarGrey::SCADA::TableColumnInfo* columns, size_t count) override;

	private:
		WarGrey::SCADA::Syslog* logger;
		sqlite3_trace_f xCallback;

	private:
		std::shared_mutex section;
		WarGrey::SCADA::SQLite3* employee;
	};
}
