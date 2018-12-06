#pragma once

#include "dbsystem.hpp"
#include "sqlite3/ffi.hpp"

#include "dirotation.hpp"

namespace WarGrey::SCADA {
	private class RotativeSQLite3 abstract : public WarGrey::SCADA::ISQLite3, public WarGrey::SCADA::IRotativeDirectory {
	public:
		virtual ~RotativeSQLite3() noexcept;

		RotativeSQLite3(Platform::String^ dbdirname, WarGrey::SCADA::Syslog* logger = nullptr,
			WarGrey::SCADA::RotationPeriod period = RotationPeriod::Daily, unsigned int period_count = 1U,
			Platform::String^ file_prefix = nullptr, Platform::String^ file_suffix = ".db",
			sqlite3_trace_f xCallback = nullptr);

	public:
		std::string filename(const char* dbname = "main") override;
		std::list<std::string> list_tables() override;
		bool table_exists(const std::string& tablename) override;
		std::string last_error_message() override;
		WarGrey::SCADA::IPreparedStatement* prepare(const std::string& sql) override;

	public:
		std::list<WarGrey::SCADA::SQliteTableInfo> table_info(const char* name) override;
		void set_busy_handler(sqlite3_busy_handler_f handler = nullptr, void* args = nullptr) override;
		void set_busy_handler(int timeout_ms) override;

	public:
		int changes(bool total = false) override;
		int64 last_insert_rowid() override;
		int last_errno(int* extended_errno) override;

	public:
		bool ready();

	protected:
		virtual void on_database_rotated(WarGrey::SCADA::SQLite3* prev_sqlite3, WarGrey::SCADA::SQLite3* current_sqlite3, long long timepoint) = 0;

	protected:
		void on_file_rotated(Windows::Storage::StorageFile^ prev, Windows::Storage::StorageFile^ current, long long timepoint) override;

	private:
		void lockfree_previous_connection();

	private:
		WarGrey::SCADA::SQLite3* prev_employee;
		WarGrey::SCADA::SQLite3* employee;
		WarGrey::SCADA::Syslog* logger;
		sqlite3_trace_f xCallback;

	private:
		sqlite3_busy_handler_f busy_handler;
		void* busy_args;
		int busy_timeout;
	};
}
