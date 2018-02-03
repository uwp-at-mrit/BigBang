#include "snip.hpp"

using namespace WarGrey::SCADA;

ISnip::~ISnip() {
	if (this->info != nullptr) {
		delete this->info;
		this->info = nullptr;
	}
}
