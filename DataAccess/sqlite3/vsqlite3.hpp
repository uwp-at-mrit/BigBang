#pragma once

#include "vsql.hpp"

namespace WarGrey::SCADA {
	private class VirtualSQLite3 final : public WarGrey::SCADA::IVirtualSQL {
	public:
		VirtualSQLite3(WarGrey::SCADA::TableColumnInfo* columns, size_t count, int libversion);

	public:
		Platform::String^ create_table(Platform::String^ tablename, Platform::String^ pks[], size_t pkc, bool if_not_exists) override;
		Platform::String^ insert_into(Platform::String^ tablename, bool replace) override;
		Platform::String^ select_from(Platform::String^ tablename, unsigned int limit, unsigned int offset) override;
		Platform::String^ drop_table(Platform::String^ tablename) override;

	private:
		int version;
	};
}
