#pragma once

#include "sqlite3/vsqlite3.hpp"

#include "string.hpp"

using namespace WarGrey::SCADA;

static inline const wchar_t* sqlite_type_map(SDT dt) {
	return dt.ToString()->Data();
}

static std::string columns_join(const char* prefix, const char* separator, const char* suffix, TableColumnInfo cols[], size_t c) {
	std::string sql = prefix;

	for (size_t i = 0; i < c; i++) {
		sql += "\"";
		sql += cols[i].name;
		sql += "\"";
		sql += ((i < c - 1) ? separator : suffix);
	}

	return sql;
}

static std::string columns_join(const char* prefix, const char* separator, const char* suffix, const char* cols[], size_t c) {
	std::string sql = prefix;

	for (size_t i = 0; i < c; i++) {
		sql += "\"";
		sql += cols[i];
		sql += "\"";
		sql += ((i < c - 1) ? separator : suffix);
	}

	return sql;
}

/*************************************************************************************************/
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
		std::string column = make_nstring("\"%s\" %S", this->columns[i].name, sqlite_type_map(this->columns[i].type));
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
				sql += "\"";
				sql += this->columns[i].name;
				sql += "\"";
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
		sql += "\"";
		sql += this->columns[i - 1].name;
		sql += "\"";
		sql += ((i < this->count) ? ", " : ") VALUES(");
		parameters += "?";
		parameters += ((i < this->count) ? ", " : ");");
	}

	sql += parameters;

	return sql;
}

std::string VirtualSQLite3::select_from(const char* tablename, const char* order_by, bool asc, uint64 limit, uint64 offset) {
	std::string sql = columns_join("SELECT ", ", ", " ", this->columns, this->count);
	std::string maybe_order_by = ((order_by == nullptr)? " " : make_nstring(" ORDER BY %s %s ", order_by, (asc ? "ASC" : "DESC")));

	sql += make_nstring("FROM %s%sLIMIT %d OFFSET %d;",
		tablename, maybe_order_by.c_str(), ((limit == 0) ? -1 : limit), offset);

	return sql;
}

std::string VirtualSQLite3::select_from(const char* tablename, const char* order_by, bool asc, const char* primary_keys[], size_t pk_count, uint64 limit, uint64 offset) {
	std::string sql = columns_join("SELECT ", ", ", " ", primary_keys, pk_count);
	std::string maybe_order_by = ((order_by == nullptr) ? " " : make_nstring(" ORDER BY %s %s ", order_by, (asc ? "ASC" : "DESC")));

	sql += make_nstring("FROM %s%sLIMIT %d OFFSET %d;",
		tablename, maybe_order_by.c_str(), ((limit == 0) ? -1 : limit), offset);

	return sql;
}

std::string VirtualSQLite3::seek_from(const char* tablename, const char* rowids[], size_t ric) {
	std::string sql = columns_join("SELECT ", ", ", " ", this->columns, this->count);

	sql += make_nstring("FROM %s ", tablename);
	sql += columns_join("WHERE ", " = ? AND ", " = ?;", rowids, ric);
	
	return sql;
}

std::string VirtualSQLite3::update_set(const char* tablename, const char* rowids[], size_t ric) {
	std::string sql = make_nstring("UPDATE %s SET ", tablename);

	for (size_t i = 0; i < this->count; i++) {
		if (!db_column_primary(this->columns[i])) {
			sql += "\"";
			sql += this->columns[i].name;
			sql += "\"";
			sql += ((i < this->count - 1) ? " = ?, " : " = ? ");
		}
	}

	sql += columns_join("WHERE ", " = ? AND ", " = ?;", rowids, ric);

	return sql;
}

std::string VirtualSQLite3::delete_from(const char* tablename, const char* rowids[], size_t ric) {
	std::string sql = make_nstring("DELETE FROM %s ", tablename);

	sql += columns_join("WHERE ", " = ? AND ", " = ?;", rowids, ric);

	return sql;
}

std::string VirtualSQLite3::drop_table(const char* tablename) {
	return make_nstring("DROP TABLE %s;", tablename);
}

/*************************************************************************************************/
std::string VirtualSQLite3::table_aggregate(const char* table, const char* function, const char* column, bool distinct) {
	const char* colname = (column == nullptr) ? this->identity_column_name() : column;
	const char* sql = (distinct ? "SELECT %s(DISTINCT %s) FROM %s;" : "SELECT %s(%s) FROM %s;");
	
	return make_nstring(sql, function, colname, table);
}

std::string VirtualSQLite3::table_average(const char* table, const char* column, bool distinct) {
	return this->table_aggregate(table, "avg", column, distinct);
}

std::string VirtualSQLite3::table_count(const char* table, const char* column, bool distinct) {
	if (column == nullptr) {
		return make_nstring("SELECT count(*) FROM %s;", table);
	} else {
		return this->table_aggregate(table, "count", column, distinct);
	}
}

std::string VirtualSQLite3::table_max(const char* table, const char* column, bool distinct) {
	return this->table_aggregate(table, "max", column, distinct);
}

std::string VirtualSQLite3::table_min(const char* table, const char* column, bool distinct) {
	return this->table_aggregate(table, "min", column, distinct);
}

std::string VirtualSQLite3::table_sum(const char* table, const char* column, bool distinct) {
	return this->table_aggregate(table, "sum", column, distinct);
}
