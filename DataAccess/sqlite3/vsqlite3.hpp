#pragma once

#include "vsql.hpp"

namespace WarGrey::SCADA {
	private class VirtualSQLite3 final : public WarGrey::SCADA::IVirtualSQL {
	public:
		VirtualSQLite3(WarGrey::SCADA::TableColumnInfo* columns, size_t count, int libversion);

	public:
		std::string create_table(const char* tablename, const char* pks[], size_t pkc, bool if_not_exists = true) override;
		std::string insert_into(const char* tablename, bool replace = false) override;
		std::string select_from(const char* tablename, unsigned int limit = 0, unsigned int offset = 0) override;
		std::string select_from(const char* tablename, const char* cols[], size_t count, unsigned int limit = 0, unsigned int offset = 0) override;
		std::string seek_from(const char* tablename, const char* pks[], size_t pkc) override;
		std::string update_set(const char* tablename, const char* primary_keys[], size_t pk_count) override;
		std::string delete_from(const char* tablename, const char* primary_keys[], size_t pk_count) override;
		std::string drop_table(const char* tablename) override;

	private:
		int version;
	};
}
