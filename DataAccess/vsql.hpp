#pragma once

#include "dbtypes.hpp"

namespace WarGrey::SCADA {
	static const unsigned int DB_PRIMARY_KEY = 0b0001;
	static const unsigned int DB_UNIQUE = 0b0010;
	static const unsigned int DB_NOT_NULL = 0b0100;

	private struct TableColumnInfo {
		const char* name;
		WarGrey::SCADA::SDT type;
		const char* dflt_value;
		unsigned int flags;
	};

	bool db_column_primary(TableColumnInfo& info);
	bool db_column_notnull(TableColumnInfo& info);
	bool db_column_unique(TableColumnInfo& info);

	private class IVirtualSQL abstract {
	public:
		virtual ~IVirtualSQL() noexcept;
		IVirtualSQL(WarGrey::SCADA::TableColumnInfo* columns, size_t count);

	public:
		virtual std::string create_table(const char* table, const char* primary_keys[], size_t pk_count, bool if_not_exists = true) = 0;
		virtual std::string insert_into(const char* table, bool replace = false) = 0;
		virtual std::string select_from(const char* table, const char* order_by, bool asc, uint64 limit = 0U, uint64 offset = 0U) = 0;
		virtual std::string select_from(const char* table, const char* order_by, bool asc, const char* primary_keys[], size_t pk_count, uint64 limit = 0U, uint64 offset = 0U) = 0;
		virtual std::string seek_from(const char* table, const char* primary_keys[], size_t pk_count) = 0;
		virtual std::string update_set(const char* table, const char* primary_keys[], size_t pk_count) = 0;
		virtual std::string delete_from(const char* table, const char* primary_keys[], size_t pk_count) = 0;
		virtual std::string drop_table(const char* table) = 0;

	public:
		virtual std::string table_average(const char* table, const char* column, bool distinct = false) = 0;
		virtual std::string table_count(const char* table, const char* column, bool distinct = false) = 0;
		virtual std::string table_max(const char* table, const char* column, bool distinct = false) = 0;
		virtual std::string table_min(const char* table, const char* column, bool distinct = false) = 0;
		virtual std::string table_sum(const char* table, const char* column, bool distinct = false) = 0;

	protected:
		const char* identity_column_name();

	protected:
		WarGrey::SCADA::TableColumnInfo* columns;
		size_t count;
	};
}
