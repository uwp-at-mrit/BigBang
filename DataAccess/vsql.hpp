#pragma once

#include "object.hpp"

namespace WarGrey::SCADA {
	private struct TableColumnInfo {
		Platform::String^ name;
		Platform::String^ type;
		bool notnull;
		bool unique;
		bool primary;
		Platform::String^ dflt_value;
	};

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
