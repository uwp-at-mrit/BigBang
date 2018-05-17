#include "dbsystem.hpp"

using namespace WarGrey::SCADA;

DBSystem::DBSystem(Syslog* logger) : logger(logger) {
	if (this->logger == nullptr) {
		this->logger = make_system_logger(Log::Error, "DBSystem");
	}

	this->logger->reference();
}

DBSystem::~DBSystem() {
	this->logger->destroy();
}

Syslog* DBSystem::get_logger() {
	return this->logger;
}
