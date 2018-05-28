#pragma once

#include "vsql.hpp"

namespace WarGrey::SCADA {
	private class VirtualSQLite3 final : public WarGrey::SCADA::IVirtualSQL {
	public:
		VirtualSQLite3(WarGrey::SCADA::TableColumnInfo* columns, size_t count, int libversion);

	public:
		std::string create_table(const char* tablename, const char* pks[], size_t pkc, bool if_not_exists) override;
		std::string insert_into(const char* tablename, bool replace) override;
		std::string select_from(const char* tablename, unsigned int limit, unsigned int offset) override;
		std::string seek_from(const char* tablename, const char* pks[], size_t pkc) override;
		std::string drop_table(const char* tablename) override;

	private:
		int version;
	};
}
