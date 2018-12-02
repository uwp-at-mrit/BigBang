#include "sqlite3/rotation.hpp"
#include "sqlite3/vsqlite3.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Storage;

/*************************************************************************************************/
RotativeSQLite3::RotativeSQLite3(Platform::String^ dbdir, Syslog* logger, RotationPeriod period, unsigned int period_count
	, Platform::String^ file_prefix, Platform::String^ file_suffix, sqlite3_trace_f xCallback)
	: ISQLite3(logger), IRotativeDirectory(dbdir, file_prefix, file_suffix, period, period_count), xCallback(xCallback) {

	this->logger = this->get_logger();
	this->logger->reference();

	this->employee = nullptr;
}

RotativeSQLite3::~RotativeSQLite3() {
	if (this->employee != nullptr) {
		delete this->employee;
	}

	this->logger->destroy();
}

bool RotativeSQLite3::ready() {
	return (this->employee != nullptr);
}

void RotativeSQLite3::on_file_rotated(StorageFile^ prev_db, StorageFile^ current_db) {
	SQLite3* new_employee = new SQLite3(current_db->Path->Data(), this->logger, this->xCallback);
	SQLite3* prev_employee = this->employee;

	this->on_database_rotated(prev_employee, new_employee);
	this->employee = new_employee;

	if (prev_employee != nullptr) {
		delete prev_employee;
	}
}

std::list<std::string> RotativeSQLite3::list_tables() {
	std::list<std::string> tables;

	if (this->ready()) {
		tables = this->employee->list_tables();
	}

	return tables;
}

bool RotativeSQLite3::table_exists(const std::string& tablename) {
	bool exists = false;

	if (this->ready()) {
		exists = this->employee->table_exists(tablename);
	}

	return exists;
}

std::string RotativeSQLite3::get_last_error_message() {
	std::string errmsg;
	
	if (this->ready()) {
		errmsg = this->employee->get_last_error_message();
	}

	return errmsg;
}

IPreparedStatement* RotativeSQLite3::prepare(const std::string& sql) {
	IPreparedStatement* statement = nullptr;

	if (this->ready()) {
		this->employee->prepare(sql);
	}

	return statement;
}

std::list<WarGrey::SCADA::SQliteTableInfo> RotativeSQLite3::table_info(const char* name) {
	return this->employee->table_info(name);
}

std::string RotativeSQLite3::filename(const char* dbname) {
	return this->employee->filename(dbname);
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
