#include "hv/playctrl.hpp"

#include "win32.hpp"
#include "box.hpp"
#include "string.hpp"

using namespace WarGrey::SCADA;

static HMODULE hv_playctrl = nullptr;
static int references = 0;

static void load_hv_playctrl(Syslog* logger) {
	if (hv_playctrl == nullptr) {
		hv_playctrl = win32_load_foreign_library("PlayCtrl_V7.3.5.20\\PlayCtrl.dll", logger);
	}
}

static void unload_hv_playctrl(Syslog* logger) {
	if (hv_playctrl != nullptr) {
		references -= 1;

		if (references <= 0) {
			win32_unload_foreign_library(hv_playctrl, logger);
			hv_playctrl = nullptr;
		}
	}
}

/*************************************************************************************************/
HikVisionPlayCtrl::HikVisionPlayCtrl(Syslog* syslog) : logger(syslog) {
	if (this->logger == nullptr) {
		this->logger = make_system_logger("HikVision");
	}

	this->logger->reference();

	load_hv_playctrl(this->logger);
}

HikVisionPlayCtrl::~HikVisionPlayCtrl() {
	unload_hv_playctrl(this->logger);

	this->logger->destroy();
}
