#include "sqlite3/rotation.hpp"
#include "sqlite3/vsqlite3.hpp"

using namespace WarGrey::SCADA;

RotativeSQLite3::RotativeSQLite3(const wchar_t* dbdir, WarGrey::SCADA::Syslog* logger, sqlite3_trace_f xCallback)
	: ISQLite3(logger), xCallback(xCallback) {

	this->logger = this->get_logger();
	this->logger->reference();

	this->employee = new SQLite3(nullptr, this->logger, this->xCallback);
}

RotativeSQLite3::~RotativeSQLite3() {
	if (this->employee != nullptr) {
		delete this->employee;
	}

	this->logger->destroy();
}

std::list<std::string> RotativeSQLite3::list_tables() {
	return this->employee->list_tables();
}

bool RotativeSQLite3::table_exists(const std::string& tablename) {
	return this->employee->table_exists(tablename);
}

std::string RotativeSQLite3::get_last_error_message() {
	return this->employee->get_last_error_message();
}

IPreparedStatement* RotativeSQLite3::prepare(const std::string& sql) {
	return this->employee->prepare(sql);
}

std::list<WarGrey::SCADA::SQliteTableInfo> RotativeSQLite3::table_info(const char* name) {
	return this->employee->table_info(name);
}

int RotativeSQLite3::libversion() {
	return this->employee->libversion();
}

int RotativeSQLite3::changes(bool total) {
	return this->employee->changes(total);
}

int64 RotativeSQLite3::last_insert_rowid() {
	return this->employee->last_insert_rowid();
}

IVirtualSQL* RotativeSQLite3::new_sql_factory(TableColumnInfo* columns, size_t count) {
	return new VirtualSQLite3(columns, count, this->libversion());
}
