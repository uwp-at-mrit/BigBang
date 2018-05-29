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
		virtual std::string create_table(const char* tablename, const char* primary_keys[], size_t pk_count, bool if_not_exists = true) = 0;
		virtual std::string insert_into(const char* tablename, bool replace = false) = 0;
		virtual std::string select_from(const char* tablename, unsigned int limit = 0, unsigned int offset = 0) = 0;
		virtual std::string select_from(const char* tablename, const char* cols[], size_t count, unsigned int limit = 0, unsigned int offset = 0) = 0;
		virtual std::string seek_from(const char* tablename, const char* primary_keys[], size_t pk_count) = 0;
		virtual std::string delete_from(const char* tablename, const char* primary_keys[], size_t pk_count) = 0;
		virtual std::string drop_table(const char* tablename) = 0;

		template<size_t N>
		std::string create_table(const char* tablename, const char* (&primary_keys)[N], bool if_not_exists) {
			return this->create_table(tablename, primary_keys, N, if_not_exists);
		}

		template<size_t N>
		std::string select_from(const char* tablename, const char* (&cols)[N], unsigned int limit = 0, unsigned int offset = 0) {
			return this->select_from(tablename, cols, N, limit, offset);
		}

		template<size_t N>
		std::string seek_from(const char* tablename, const char* (&primary_keys)[N]) {
			return this->seek_from(tablename, primary_keys, N);
		}

		template<size_t N>
		std::string delete_from(const char* tablename, const char* (&primary_keys)[N]) {
			return this->delete_from(tablename, primary_keys, N);
		}

	protected:
		WarGrey::SCADA::TableColumnInfo* columns;
		size_t count;
	};
}
