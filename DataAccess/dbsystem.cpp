#include "dbsystem.hpp"

#include "string.hpp"

using namespace WarGrey::SCADA;

#define DBMaybe(Type, stmt, column_value, cid) \
    (stmt->column_is_null(cid) ? std::nullopt : std::optional<Type>(stmt->column_value(cid)))

#define QueryValue(sql, type, column_maybe_value, defval) \
    IPreparedStatement* stmt = this->prepare(sql); \
    type maybe_v = defval; \
\
    if (stmt != nullptr) { \
	    this->exec(stmt); \
	    maybe_v = stmt->column_maybe_value(0); \
\
	    delete stmt; \
    } \
\
    return maybe_v;

#define ImplementQueryValue(type, value, defval) \
std::optional<type> IDBSystem::query_maybe_##value(const std::string& sql) { \
	QueryValue(sql, std::optional<type>, column_maybe_##value, defval); \
} \
\
std::optional<type> IDBSystem::query_maybe_##value(const char* sql, ...) { \
	VSNPRINT(stmt, sql); \
\
	return this->query_maybe_##value(stmt); \
} \
\
type IDBSystem::query_##value(const std::string& sql) { \
	QueryValue(sql, type, column_##value, defval); \
} \
\
type IDBSystem::query_##value(const char* sql, ...) { \
	VSNPRINT(stmt, sql); \
\
	return this->query_##value(stmt); \
}

IDBSystem::IDBSystem(DBMS dbms, Syslog* logger) : IDBObject(dbms), logger(logger) {
	if (this->logger == nullptr) {
		this->logger = make_system_logger("DBSystem");
	}

	this->logger->reference();
}

IDBSystem::~IDBSystem() {
	this->logger->destroy();

	for (auto it = this->factories.begin(); it != this->factories.end(); it++) {
		delete it->second;
	}
}

IVirtualSQL* IDBSystem::make_sql_factory(TableColumnInfo* columns, size_t count) {
	IVirtualSQL* vsql = nullptr;
	auto it = factories.find(columns);

	if (it == factories.end()) {
		vsql = this->new_sql_factory(columns, count);
		factories.insert(std::pair<TableColumnInfo*, IVirtualSQL*>(columns, vsql));
	} else {
		vsql = it->second;
	}

	return vsql;
}

IPreparedStatement* IDBSystem::prepare(const char* sql, ...) {
	VSNPRINT(raw, sql);

	return this->prepare(raw);
}

void IDBSystem::exec(IPreparedStatement* stmt) {
	if (this->dbsystem() == stmt->dbsystem()) {
		stmt->step(nullptr, "exec::step");
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

ImplementQueryValue(std::string, text, "")
ImplementQueryValue(int32, int32, 0)
ImplementQueryValue(int64, int64, 0)
ImplementQueryValue(double, double, 0.0)

bool IDBSystem::table_exists(const std::string& tablename) {
	std::list<std::string> all = this->list_tables();
	bool found = false;

	for (auto it = all.begin(); it != all.end(); it++) {
		if ((*it).compare(tablename) == 0) {
			found = true;
			break;
		}
	}

	return found;
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
	this->get_logger()->log_message(level, L"%S", this->last_error_message().c_str());
}

void IDBSystem::log(const std::string& message_prefix, Log level) {
	this->get_logger()->log_message(level, L"%S: %S",
		message_prefix.c_str(),
		this->last_error_message().c_str());
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
