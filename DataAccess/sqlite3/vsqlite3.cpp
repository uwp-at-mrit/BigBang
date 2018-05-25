#pragma once

#include "sqlite3/vsqlite3.hpp"

#include "string.hpp"

using namespace WarGrey::SCADA;

static Platform::String^ sqlite_type_map(SDT dt) {
	return dt.ToString();
}

VirtualSQLite3::VirtualSQLite3(TableColumnInfo* columns, size_t count, int version) :
	IVirtualSQL(columns, count), version(version) {}

Platform::String^ VirtualSQLite3::create_table(Platform::String^ tablename, Platform::String^ rowids[], size_t ric, bool silent) {
	Platform::String^ sql = make_string(L"CREATE %s %s(", (silent ? L"TABLE IF NOT EXISTS" : L"TABLE"), tablename->Data());
	size_t pk_count = 0;
	bool rowid_is_defined = false;

	for (size_t i = 0; i < this->count; i++) {
		pk_count += (db_column_primary(this->columns[i]) ? 1 : 0);

		if (rowid_is_defined) {
			for (size_t ridx = 0; ridx < ric; ridx++) {
				if (this->columns[i].name->Equals(rowids[ridx++])) {
					rowid_is_defined = true;
				}
			}
		}
	}

	for (size_t i = 0; i < count; i++) {
		Platform::String^ column = this->columns[i].name + " " + sqlite_type_map(this->columns[i].type);
		bool nnil = db_column_notnull(this->columns[i]);
		bool uniq = db_column_unique(this->columns[i]);

		if (db_column_primary(this->columns[i])) {
			if (pk_count == 1) { column += " PRIMARY KEY"; }
		} else if (nnil || uniq) {
			if (uniq) { column += " UNIQUE"; }
			if (nnil) { column += " NOT NULL"; }
		}

		sql += (column + ((i < count - 1) ? ", " : ""));
	}

	if (pk_count > 0) {
		sql += ", PRIMARY KEY(";

		for (size_t i = 0; i < this->count; i++) {
			if (db_column_primary(this->columns[i])) {
				sql += (this->columns[i].name + ((pk_count == 1) ? ")" : ", "));
				pk_count -= 1;
			}
		}
	}

	sql += ")" + (((!rowid_is_defined) && (this->version >= 3008002)) ? " WITHOUT ROWID;" : ";");

	return sql;
}
