#pragma once

#include "vsql.hpp"

namespace WarGrey::SCADA {
	private class VirtualSQLite3 final : public WarGrey::SCADA::IVirtualSQL {
	public:
		VirtualSQLite3(WarGrey::SCADA::TableColumnInfo* columns, size_t count, int libversion);

	public:
		std::string create_table(const char* tablename, const char* pks[], size_t pkc, bool if_not_exists = true) override;
		std::string insert_into(const char* tablename, bool replace = false) override;
		std::string select_from(const char* tablename, const char* order_by, bool asc, uint64 limit = 0U, uint64 offset = 0U) override;
		std::string select_from(const char* tablename, const char* order_by, bool asc, const char* cols[], size_t count, uint64 limit = 0U, uint64 offset = 0U) override;
		std::string seek_from(const char* tablename, const char* pks[], size_t pkc) override;
		std::string update_set(const char* tablename, const char* primary_keys[], size_t pk_count) override;
		std::string delete_from(const char* tablename, const char* primary_keys[], size_t pk_count) override;
		std::string drop_table(const char* tablename) override;

	public:
		std::string table_average(const char* table, const char* column, bool distinct = false) override;
		std::string table_count(const char* table, const char* column, bool distinct = false) override;
		std::string table_max(const char* table, const char* column, bool distinct = false) override;
		std::string table_min(const char* table, const char* column, bool distinct = false) override;
		std::string table_sum(const char* table, const char* column, bool distinct = false) override;

	private:
		std::string table_aggregate(const char* table, const char* function, const char* column, bool distinct);

	private:
		int version;
	};
}
