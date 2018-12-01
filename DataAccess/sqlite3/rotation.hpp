#pragma once

#include "dbsystem.hpp"
#include "sqlite3/dll.hpp"

#include "dirotation.hpp"

namespace WarGrey::SCADA {
	private class RotativeSQLite3 abstract : public WarGrey::SCADA::ISQLite3, public WarGrey::SCADA::IRotativeDirectory {
	public:
		virtual ~RotativeSQLite3() noexcept;

		RotativeSQLite3(Platform::String^ dbdirname, WarGrey::SCADA::Syslog* logger = nullptr,
			WarGrey::SCADA::RotationPeriod period = RotationPeriod::Daily, unsigned int period_count = 1U,
			Platform::String^ file_prefix = nullptr, Platform::String^ file_suffix = ".db",
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
		virtual void on_database_rotated(WarGrey::SCADA::SQLite3* prev_sqlite3, WarGrey::SCADA::SQLite3* current_sqlite3) = 0;

	protected:
		WarGrey::SCADA::IVirtualSQL* new_sql_factory(WarGrey::SCADA::TableColumnInfo* columns, size_t count) override;

	protected:
		void on_file_rotated(Windows::Storage::StorageFile^ prev, Windows::Storage::StorageFile^ current) override;

	private:
		void lockfree_prev_employee();

	private:
		WarGrey::SCADA::SQLite3* prev_employee;
		WarGrey::SCADA::SQLite3* employee;
		WarGrey::SCADA::Syslog* logger;
		sqlite3_trace_f xCallback;
	};
}
