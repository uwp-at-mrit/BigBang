#include "snip.hpp"
#include "planet.hpp"

using namespace WarGrey::SCADA;

ISnip::~ISnip() {
	if (this->info != nullptr) {
		delete this->info;
		this->info = nullptr;
	}
}

Syslog* ISnip::get_logger() {
	Syslog* logger = nullptr;

	if (this->info != nullptr) {
		logger = this->info->master->get_logger();
	}

	return logger;
}
