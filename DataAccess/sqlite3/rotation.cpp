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

	this->prev_employee = nullptr;
	this->employee = new SQLite3(nullptr, this->get_logger());
}

RotativeSQLite3::~RotativeSQLite3() {
	this->lockfree_previous_connection();
	delete this->employee;
	this->logger->destroy();
}

bool RotativeSQLite3::ready() {
	return (this->employee != nullptr);
}

void RotativeSQLite3::on_file_rotated(StorageFile^ prev_db, StorageFile^ current_db, long long timepoint) {
	SQLite3* new_employee = new SQLite3(current_db->Path->Data(), this->logger, this->xCallback);

	if (this->busy_timeout > 0) {
		new_employee->set_busy_handler(this->busy_timeout);
	} else {
		new_employee->set_busy_handler(this->busy_handler, this->busy_args);
	}

	this->lockfree_previous_connection();

	this->prev_employee = this->employee;
	this->on_database_rotated(this->prev_employee, new_employee, timepoint);
	this->employee = new_employee;

	// destruct last connection until next rotation for lockfree purpose
}

std::list<std::string> RotativeSQLite3::list_tables() {
	return this->employee->list_tables();
}

bool RotativeSQLite3::table_exists(const std::string& tablename) {
	return this->employee->table_exists(tablename);
}

std::string RotativeSQLite3::last_error_message() {
	return this->employee->last_error_message();
}

IPreparedStatement* RotativeSQLite3::prepare(const std::string& sql) {
	return this->employee->prepare(sql);
}

std::list<WarGrey::SCADA::SQliteTableInfo> RotativeSQLite3::table_info(const char* name) {
	return this->employee->table_info(name);
}

void RotativeSQLite3::set_busy_handler(sqlite3_busy_handler_f handler, void* args) {
	this->busy_timeout = 0;
	this->busy_handler = handler;
	this->busy_args = args;
	this->employee->set_busy_handler(this->busy_handler, this->busy_args);
}

void RotativeSQLite3::set_busy_handler(int timeout_ms) {
	this->busy_handler = nullptr;
	this->busy_timeout = timeout_ms;
	this->employee->set_busy_handler(this->busy_timeout);
}

std::string RotativeSQLite3::filename(const char* dbname) {
	return this->employee->filename(dbname);
}

int RotativeSQLite3::changes(bool total) {
	return this->employee->changes(total);
}

int64 RotativeSQLite3::last_insert_rowid() {
	return this->employee->last_insert_rowid();
}

int RotativeSQLite3::last_errno(int* extended_errno) {
	return this->employee->last_errno(extended_errno);
}

void RotativeSQLite3::lockfree_previous_connection() {
	if (this->prev_employee != nullptr) {
		delete prev_employee;
		this->prev_employee = nullptr;
	}
}
