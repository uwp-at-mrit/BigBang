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
		virtual std::string create_table(const char* tablename,
			const char* primary_keys[], size_t pk_count,
			bool if_not_exists) = 0;

		virtual std::string insert_into(const char* tablename, bool replace) = 0;
		virtual std::string select_from(const char* tablename, unsigned int limit = 0, unsigned int offset = 0) = 0;
		virtual std::string drop_table(const char* tablename) = 0;

	protected:
		WarGrey::SCADA::TableColumnInfo* columns;
		size_t count;
	};
}
