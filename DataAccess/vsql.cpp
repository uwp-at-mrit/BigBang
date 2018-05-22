#include "vsql.hpp"

using namespace WarGrey::SCADA;

IVirtualSQL::IVirtualSQL(TableColumnInfo* columns, size_t count) {
	this->columns = new TableColumnInfo[count];

	for (size_t idx = 0; idx < count; idx++) {
		this->columns[idx] = columns[idx];
	}
}

IVirtualSQL::~IVirtualSQL() {
	delete[] this->columns;
}
