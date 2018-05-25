#include "dbsystem.hpp"

#include "string.hpp"

using namespace WarGrey::SCADA;

IDBSystem::IDBSystem(Syslog* logger) : logger(logger) {
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

void IDBSystem::exec(const wchar_t* sql, ...) {
	VSWPRINT(raw, sql);

	this->exec(raw);
}

void IDBSystem::report_error(Platform::String^ msg_prefix) {
	this->log(msg_prefix, Log::Error);
}

void IDBSystem::report_error(const wchar_t* format, ...) {
	VSWPRINT(message, format);
	this->log(message, Log::Error);
}

void IDBSystem::report_warning(Platform::String^ msg_prefix) {
	this->log(msg_prefix, Log::Warning);
}

void IDBSystem::report_warning(const wchar_t* format, ...) {
	VSWPRINT(message, format);
	this->log(message, Log::Warning);
}

void IDBSystem::log(Platform::String^ message_prefix, Log level) {
	const wchar_t* message = this->get_last_error_message();

	if (message_prefix == nullptr) {
		this->get_logger()->log_message(level, L"%s", message);
	} else {
		this->get_logger()->log_message(level, L"%s: %s", message_prefix->Data(), message);
	}
}

Syslog* IDBSystem::get_logger() {
	return this->logger;
}
