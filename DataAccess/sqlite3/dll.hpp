#pragma once

#include <cstdint>
#include <list>

#include "dbsystem.hpp"
#include "syslog.hpp"

namespace WarGrey::SCADA {
	typedef void sqlite3_t;
	typedef void sqlite3_stmt_t;

#define SQLITE_TRACE_STMT    0x01
#define SQLITE_TRACE_PROFILE 0x02
#define SQLITE_TRACE_ROW     0x04
#define SQLITE_TRACE_CLOSE   0x08

	typedef int (*sqlite3_trace_f)(unsigned int, void*, void*, void*);

	private enum class SQLiteDataType { Integer = 1, Float = 2, Text = 3, Bytes = 4, Null = 5 };

	private struct SQliteTableInfo { // for pragma.table_info
		int cid;
		std::string name;
		std::string type;
		bool notnull;
		std::string dflt_value;
		int pk;
	};

	int sqlite3_default_trace_callback(unsigned int reason, void* pCxt, void* P, void* X);

	private class ISQLite3 abstract : public WarGrey::SCADA::IDBSystem {
	public:
		virtual ~ISQLite3() noexcept;

		ISQLite3(WarGrey::SCADA::Syslog* logger);

	public:
		virtual std::list<WarGrey::SCADA::SQliteTableInfo> table_info(const char* name) = 0;

	public:
		virtual int libversion() = 0;
		virtual int changes(bool total = false) = 0;
		virtual int64 last_insert_rowid() = 0;
	};

	private class SQLiteStatement : public WarGrey::SCADA::IPreparedStatement {
	public:
		virtual ~SQLiteStatement() noexcept;

		SQLiteStatement(WarGrey::SCADA::ISQLite3* db, sqlite3_stmt_t* stmt);

	public:
		unsigned int parameter_count() override;
		void bind_parameter(unsigned int pid_starts_with_0) override;
		void bind_parameter(unsigned int pid_starts_with_0, int32 v) override;
		void bind_parameter(unsigned int pid_starts_with_0, int64 v) override;
		void bind_parameter(unsigned int pid_starts_with_0, double v) override;
		void bind_parameter(unsigned int pid_starts_with_0, const char* text) override;

	public:
		std::string column_text(unsigned int cid_starts_with_0) override;
		int32 column_int32(unsigned int cid_starts_with_0) override;
		int64 column_int64(unsigned int cid_starts_with_0) override;
		double column_double(unsigned int cid_starts_with_0) override;

	public:
		int column_data_count() override;
		bool column_is_null(unsigned int cid_starts_with_0) override;
		std::string column_database_name(unsigned int cid_starts_with_0) override;
		std::string column_table_name(unsigned int cid_starts_with_0) override;
		std::string column_name(unsigned int cid_starts_with_0) override;
		std::string column_decltype(unsigned int cid_starts_with_0) override;
		SQLiteDataType column_type(unsigned int cid_starts_with_0);

	public:
		std::string description(bool expand = true) override;
		bool step(int* data_count = nullptr, const char* error_src = "step") override;
		void reset(bool reset_bindings = true) override;
		void clear_bindings() override;

	private:
		WarGrey::SCADA::ISQLite3* master;
		WarGrey::SCADA::sqlite3_stmt_t* stmt;
	};

	private class SQLite3 : public WarGrey::SCADA::ISQLite3 {
	public:
		virtual ~SQLite3() noexcept;

		SQLite3(const wchar_t* dbfile = nullptr, WarGrey::SCADA::Syslog* logger = nullptr,
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
		WarGrey::SCADA::sqlite3_t* db;
	};
}
