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
		unsigned int parameter_count() override;
		void bind_parameter(unsigned int pid_starts_with_0) override;
		void bind_parameter(unsigned int pid_starts_with_0, int32 v) override;
		void bind_parameter(unsigned int pid_starts_with_0, int64 v) override;
		void bind_parameter(unsigned int pid_starts_with_0, double v) override;
		void bind_parameter(unsigned int pid_starts_with_0, const char* blob) override;
		void bind_parameter(unsigned int pid_starts_with_0, const wchar_t* text) override;

	public:
		bool step(int* data_count = nullptr, const wchar_t* error_src = L"step") override;
		int column_data_count() override;
		Platform::String^ column_database_name(unsigned int cid_starts_with_0) override;
		Platform::String^ column_table_name(unsigned int cid_starts_with_0) override;
		Platform::String^ column_name(unsigned int cid_starts_with_0) override;
		Platform::String^ column_decltype(unsigned int cid_starts_with_0) override;
		std::string column_blob(unsigned int cid_starts_with_0) override;
		Platform::String^ column_text(unsigned int cid_starts_with_0) override;
		int32 column_int32(unsigned int cid_starts_with_0) override;
		int64 column_int64(unsigned int cid_starts_with_0) override;
		double column_double(unsigned int cid_starts_with_0) override;

	public:
		void reset(bool reset_bindings = true) override;
		void clear_bindings() override;

	public:
		SQLiteDataType column_type(unsigned int cid_starts_with_0);

	private:
		WarGrey::SCADA::SQLite3* master;
		WarGrey::SCADA::sqlite3_stmt_t* stmt;
	};

	private class SQLite3 : public WarGrey::SCADA::IDBSystem {
	public:
		virtual ~SQLite3() noexcept;

		SQLite3(const wchar_t* dbfile = nullptr, WarGrey::SCADA::Syslog* logger = nullptr);

	public:
		const wchar_t* get_last_error_message() override;
		WarGrey::SCADA::IPreparedStatement* prepare(Platform::String^ sql) override;

	public:
		std::list<WarGrey::SCADA::SQliteTableInfo> table_info(const wchar_t* name);

	public:
		int changes(bool total = false);
		long last_insert_rowid();
		int libversion();

	protected:
	WarGrey::SCADA::IVirtualSQL* new_sql_factory(WarGrey::SCADA::TableColumnInfo* columns, size_t count) override;

	private:
		WarGrey::SCADA::sqlite3_t* db;
	};
}
