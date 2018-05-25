#pragma once

#include "object.hpp"
#include "dbtypes.hpp"

namespace WarGrey::SCADA {
	static const unsigned int DB_PRIMARY_KEY = 0b0001;
	static const unsigned int DB_UNIQUE = 0b0010;
	static const unsigned int DB_NOT_NULL = 0b0100;

	private struct TableColumnInfo {
		Platform::String^ name;
		WarGrey::SCADA::SDT type;
		Platform::String^ dflt_value;
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
		virtual Platform::String^ create_table(Platform::String^ tablename,
			Platform::String^ primary_keys[], size_t pk_count,
			bool if_not_exists) = 0;

	protected:
		WarGrey::SCADA::TableColumnInfo* columns;
		size_t count;
	};
}
