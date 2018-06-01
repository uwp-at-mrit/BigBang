#include "vsql.hpp"

using namespace WarGrey::SCADA;

IVirtualSQL::IVirtualSQL(TableColumnInfo* src, size_t count) : count(count) {
	this->columns = new TableColumnInfo[count];

	for (size_t idx = 0; idx < count; idx++) {
		this->columns[idx] = src[idx];
	}
}

IVirtualSQL::~IVirtualSQL() {
	if (this->columns != nullptr) {
		delete[] this->columns;
	}
}

const char* IVirtualSQL::identity_column_name() {
	const char* idcolname = this->columns[0].name;

	for (size_t idx = 0; idx < count; idx++) {
		if (db_column_primary(this->columns[idx])) {
			idcolname = this->columns[idx].name;
			break;
		}
	}
	
	return idcolname;
}

/*************************************************************************************************/
bool WarGrey::SCADA::db_column_primary(TableColumnInfo& info) {
	return ((info.flags & DB_PRIMARY_KEY) == DB_PRIMARY_KEY);
}

bool WarGrey::SCADA::db_column_notnull(TableColumnInfo& info) {
	return ((info.flags & DB_NOT_NULL) == DB_NOT_NULL);
}

bool WarGrey::SCADA::db_column_unique(TableColumnInfo& info) {
	return ((info.flags & DB_UNIQUE) == DB_UNIQUE);
}
