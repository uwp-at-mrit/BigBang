#include "sqlite3/rotation.hpp"
#include "sqlite3/vsqlite3.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Storage;

RotativeSQLite3::RotativeSQLite3(Platform::String^ dbdir, Syslog* logger, RotationPeriod period, unsigned int period_count
	, Platform::String^ file_prefix, Platform::String^ file_suffix, sqlite3_trace_f xCallback)
	: ISQLite3(logger), IRotativeDirectory(dbdir, file_prefix, file_suffix, period, period_count), xCallback(xCallback) {

	this->logger = this->get_logger();
	this->logger->reference();

	this->prev_employee = nullptr;
	this->employee = nullptr;
}

RotativeSQLite3::~RotativeSQLite3() {
	this->lockfree_prev_employee();

	if (this->employee != nullptr) {
		delete this->employee;
	}

	this->logger->destroy();
}

void RotativeSQLite3::on_file_rotated(StorageFile^ prev_db, StorageFile^ current_db) {
	SQLite3* saved_employee = this->employee;

	this->employee = new SQLite3(current_db->Path->Data(), this->logger, this->xCallback);
	this->on_database_rotated(saved_employee, this->employee);

	this->lockfree_prev_employee();
	this->prev_employee = saved_employee;
}

std::list<std::string> RotativeSQLite3::list_tables() {
	this->lockfree_prev_employee();

	return this->employee->list_tables();
}

bool RotativeSQLite3::table_exists(const std::string& tablename) {
	this->lockfree_prev_employee();

	return this->employee->table_exists(tablename);
}

std::string RotativeSQLite3::get_last_error_message() {
	this->lockfree_prev_employee();

	return this->employee->get_last_error_message();
}

IPreparedStatement* RotativeSQLite3::prepare(const std::string& sql) {
	this->lockfree_prev_employee();

	return this->employee->prepare(sql);
}

std::list<WarGrey::SCADA::SQliteTableInfo> RotativeSQLite3::table_info(const char* name) {
	this->lockfree_prev_employee();

	return this->employee->table_info(name);
}

int RotativeSQLite3::libversion() {
	this->lockfree_prev_employee();

	return this->employee->libversion();
}

int RotativeSQLite3::changes(bool total) {
	this->lockfree_prev_employee();

	return this->employee->changes(total);
}

int64 RotativeSQLite3::last_insert_rowid() {
	this->lockfree_prev_employee();

	return this->employee->last_insert_rowid();
}

IVirtualSQL* RotativeSQLite3::new_sql_factory(TableColumnInfo* columns, size_t count) {
	return new VirtualSQLite3(columns, count, this->libversion());
}

void RotativeSQLite3::lockfree_prev_employee() {
	if (this->prev_employee != nullptr) {
		delete this->prev_employee;
		this->prev_employee = nullptr;
	}
}

