#pragma once

#include <cstdint>
#include <list>

#include "dbsystem.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	typedef void sqlite3_t;
	typedef void sqlite3_stmt_t;

	private enum class SQLiteDataType { Integer = 1, Float = 2, Text = 3, Blob = 4, Null = 5 };

	private struct SQliteTableInfo { // for pragma.table_info
		int cid;
		Platform::String^ name;
		Platform::String^ type;
		bool notnull;
		Platform::String^ dflt_value;
		int pk;
	};

	class SQLite3;

	private class SQLiteStatement : public WarGrey::SCADA::IPreparedStatement {
	public:
		virtual ~SQLiteStatement() noexcept;

		SQLiteStatement(WarGrey::SCADA::SQLite3* db, sqlite3_stmt_t* stmt);

	public:
		unsigned int parameter_count();
		void bind_parameter(int pid_starts_with_0);
		void bind_parameter(int pid_starts_with_0, int32 v);
		void bind_parameter(int pid_starts_with_0, int64 v);
		void bind_parameter(int pid_starts_with_0, float v);
		void bind_parameter(int pid_starts_with_0, double v);
		void bind_parameter(int pid_starts_with_0, const char* blob);
		void bind_parameter(int pid_starts_with_0, std::string blob);
		void bind_parameter(int pid_starts_with_0, const wchar_t* text);
		void bind_parameter(int pid_starts_with_0, Platform::String^ text);

	public:
		bool step(int* data_count = nullptr);
		int column_data_count();
		Platform::String^ column_database_name(int cid_starts_with_0);
		Platform::String^ column_table_name(int cid_starts_with_0);
		Platform::String^ column_name(int cid_starts_with_0);
		Platform::String^ column_decltype(int cid_starts_with_0);
		SQLiteDataType column_type(int cid_starts_with_0);
		std::string column_blob(int cid_starts_with_0);
		Platform::String^ column_text(int cid_starts_with_0);
		int32 column_int32(int cid_starts_with_0);
		int64 column_int64(int cid_starts_with_0);
		float column_float(int cid_starts_with_0);
		double column_double(int cid_starts_with_0);

	public:
		void reset(bool reset_bindings = true);
		void clear_bindings();

	private:
		WarGrey::SCADA::SQLite3* master;
		WarGrey::SCADA::sqlite3_stmt_t* stmt;
	};

	private class SQLite3 : public WarGrey::SCADA::IDBSystem {
	public:
		virtual ~SQLite3() noexcept;

		SQLite3(const wchar_t* dbfile = nullptr, WarGrey::SCADA::Syslog* logger = nullptr);

	public:
		Platform::String^ get_name() override;
		int libversion();

	public:
		WarGrey::SCADA::SQLiteStatement* prepare(Platform::String^ sql);
		WarGrey::SCADA::SQLiteStatement* prepare(const wchar_t* sql, ...);

		void exec(WarGrey::SCADA::SQLiteStatement* stmt);
		void exec(Platform::String^ sql);
		void exec(const wchar_t* sql, ...);

	public:
		void create_table(const wchar_t* tablename,
			WarGrey::SCADA::TableColumnInfo* columns, size_t count,
			bool silent = false, bool without_rowid = false);

		template<size_t N>
		void create_table(const wchar_t* tablename
			, WarGrey::SCADA::TableColumnInfo (&columns)[N]
			, bool silent = false, bool without_rowid = false) {
			this->create_table(tablename, columns, N, silent, without_rowid);
		}

	public:
		std::list<WarGrey::SCADA::SQliteTableInfo> table_info(const wchar_t* name);

	public:
		int changes(bool total = false);
		long last_insert_rowid();

	public:
		void report_error(Platform::String^ msg_prefix = nullptr);
		void report_error(const wchar_t* format, ...);

		void report_warning(Platform::String^ msg_prefix = nullptr);
		void report_warning(const wchar_t* format, ...);

	private:
		void log(Platform::String^ msg_prefix = nullptr, WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Error);

	private:
		WarGrey::SCADA::sqlite3_t* db;
	};
}
