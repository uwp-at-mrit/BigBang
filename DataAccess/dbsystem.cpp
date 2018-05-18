#include "dbsystem.hpp"

using namespace WarGrey::SCADA;

IDBSystem::IDBSystem(Syslog* logger) : logger(logger) {
	if (this->logger == nullptr) {
		this->logger = make_system_logger("DBSystem");
	}

	this->logger->reference();
}

IDBSystem::~IDBSystem() {
	this->logger->destroy();
}

Syslog* IDBSystem::get_logger() {
	return this->logger;
}
