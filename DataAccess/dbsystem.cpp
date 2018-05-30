#include "dbsystem.hpp"

#include "string.hpp"

using namespace WarGrey::SCADA;

#define DBMaybe(Type, stmt, column_value, cid) \
    (stmt->column_is_null(cid) ? std::nullopt : std::optional<Type>(stmt->column_value(cid)))

IDBSystem::IDBSystem(Syslog* logger, DBMS dbms) : IDBObject(dbms), logger(logger) {
	if (this->logger == nullptr) {
		this->logger = make_system_logger("DBSystem");
	}

	this->logger->reference();
}

IDBSystem::~IDBSystem() {
	this->logger->destroy();

	for (auto lt = this->factories.begin(); lt != this->factories.end(); lt++) {
		delete lt->second;
	}
}

IVirtualSQL* IDBSystem::make_sql_factory(TableColumnInfo* columns, size_t count) {
	IVirtualSQL* vsql = nullptr;
	auto lt = factories.find(columns);

	if (lt == factories.end()) {
		vsql = this->new_sql_factory(columns, count);
		factories.insert(std::pair<TableColumnInfo*, IVirtualSQL*>(columns, vsql));
	} else {
		vsql = lt->second;
	}

	return vsql;
}

IPreparedStatement* IDBSystem::prepare(const char* sql, ...) {
	VSNPRINT(raw, sql);

	return this->prepare(raw);
}

void IDBSystem::exec(IPreparedStatement* stmt) {
	if (this->dbsystem() == stmt->dbsystem()) {
		stmt->step(nullptr, "exec");
	} else {
		this->get_logger()->log_message(Log::Warning, "incompatible dbsystem and statement");
	}
}

void IDBSystem::exec(const std::string& sql) {
	IPreparedStatement* stmt = this->prepare(sql);

	if (stmt != nullptr) {
		this->exec(stmt);
		delete stmt;
	}
}

void IDBSystem::exec(const char* sql, ...) {
	VSNPRINT(raw, sql);

	this->exec(raw);
}

void IDBSystem::report_error() {
	this->log(Log::Error);
}

void IDBSystem::report_error(const std::string& msg_prefix) {
	this->log(msg_prefix, Log::Error);
}

void IDBSystem::report_error(const char* format, ...) {
	VSNPRINT(message, format);
	this->log(message, Log::Error);
}

void IDBSystem::report_warning() {
	this->log(Log::Warning);
}

void IDBSystem::report_warning(const std::string& msg_prefix) {
	this->log(msg_prefix, Log::Warning);
}

void IDBSystem::report_warning(const char* format, ...) {
	VSNPRINT(message, format);
	this->log(message, Log::Warning);
}

void IDBSystem::log(Log level) {
	this->get_logger()->log_message(level, L"%S", this->get_last_error_message().c_str());
}

void IDBSystem::log(const std::string& message_prefix, Log level) {
	this->get_logger()->log_message(level, L"%S: %S",
		message_prefix.c_str(),
		this->get_last_error_message().c_str());
}

Syslog* IDBSystem::get_logger() {
	return this->logger;
}

/*************************************************************************************************/
void IPreparedStatement::bind_parameter(unsigned int pid, float v) {
	this->bind_parameter(pid, double(v));
}

void IPreparedStatement::bind_parameter(unsigned int pid, const std::string& v) {
	this->bind_parameter(pid, v.c_str());
}

void IPreparedStatement::bind_parameter(unsigned int pid, Platform::String^ v) {
	this->bind_parameter(pid, make_nstring(v).c_str());
}

float IPreparedStatement::column_float(unsigned int cid) {
	return float(this->column_double(cid));
}

std::optional<std::string> IPreparedStatement::column_maybe_text(unsigned int cid) {
	return DBMaybe(std::string, this, column_text, cid);
}

std::optional<int32> IPreparedStatement::column_maybe_int32(unsigned int cid) {
	return DBMaybe(int32, this, column_int32, cid);
}

std::optional<int64> IPreparedStatement::column_maybe_int64(unsigned int cid) {
	return DBMaybe(int64, this, column_int64, cid);
}

std::optional<double> IPreparedStatement::column_maybe_double(unsigned int cid) {
	return DBMaybe(double, this, column_double, cid);
}
