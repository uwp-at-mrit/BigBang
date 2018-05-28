#pragma once

#include "sqlite3/vsqlite3.hpp"

#include "string.hpp"

using namespace WarGrey::SCADA;

static inline const wchar_t* sqlite_type_map(SDT dt) {
	return dt.ToString()->Data();
}

VirtualSQLite3::VirtualSQLite3(TableColumnInfo* columns, size_t count, int version) :
	IVirtualSQL(columns, count), version(version) {}

std::string VirtualSQLite3::create_table(const char* tablename, const char* rowids[], size_t ric, bool silent) {
	std::string sql = make_nstring("CREATE %s %s(", (silent ? "TABLE IF NOT EXISTS" : "TABLE"), tablename);
	size_t pk_count = 0;
	bool rowid_is_defined = false;

	for (size_t i = 0; i < this->count; i++) {
		pk_count += (db_column_primary(this->columns[i]) ? 1 : 0);

		if (rowid_is_defined) {
			for (size_t ridx = 0; ridx < ric; ridx++) {
				if (strcmp(this->columns[i].name, rowids[ridx++]) == 0) {
					rowid_is_defined = true;
				}
			}
		}
	}

	for (size_t i = 0; i < count; i++) {
		std::string column = make_nstring("%s %S", this->columns[i].name, sqlite_type_map(this->columns[i].type));
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

	if (pk_count > 1) {
		sql += ", PRIMARY KEY(";

		for (size_t i = 0; i < this->count; i++) {
			if (db_column_primary(this->columns[i])) {
				sql += this->columns[i].name;
				sql += ((pk_count == 1) ? ")" : ", ");
				pk_count -= 1;
			}
		}
	}

	sql += ")";
	sql += (((!rowid_is_defined) && (this->version >= 3008002)) ? " WITHOUT ROWID;" : ";");

	return sql;
}

std::string VirtualSQLite3::insert_into(const char* tablename, bool replace) {
	std::string sql = make_nstring("INSERT %s %s (", (replace ? "OR REPLACE INTO" : "INTO"), tablename);
	std::string parameters = "";

	for (size_t i = 1; i <= this->count; i++) {
		sql += this->columns[i - 1].name;
		sql += ((i < this->count) ? ", " : ") VALUES(");
		parameters += "?";
		parameters += ((i < this->count) ? ", " : ");");
	}

	sql += parameters;

	return sql;
}

std::string VirtualSQLite3::select_from(const char* tablename, unsigned int limit, unsigned int offset) {
	std::string sql = "SELECT ";

	for (size_t i = 0; i < this->count; i++) {
		sql += this->columns[i].name;
		sql += ((i < this->count - 1) ? ", " : " ");
	}

	sql += make_nstring("FROM %s LIMIT %d OFFSET %d;", tablename, ((limit == 0) ? -1 : limit), offset);

	return sql;
}

std::string VirtualSQLite3::seek_from(const char* tablename, const char* rowids[], size_t ric) {
	std::string sql = "SELECT ";

	for (size_t i = 0; i < this->count; i++) {
		sql += this->columns[i].name;
		sql += ((i < this->count - 1) ? ", " : " ");
	}

	sql += make_nstring("FROM %s WHERE ", tablename);
	
	for (size_t i = 0; i < ric; i++) {
		sql += rowids[i];
		sql += " = ?";
		sql += ((i < ric - 1) ? " AND " : ";");
	}

	return sql;
}

std::string VirtualSQLite3::drop_table(const char* tablename) {
	return make_nstring("DROP TABLE %s;", tablename);
}
