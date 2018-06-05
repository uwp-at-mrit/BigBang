#include "hv/hcnet.hpp"

#include "usr/share/include/HCNetSDK.h"
#include "string.hpp"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
HikVisionNet::HikVisionNet(Syslog* syslog) : HikVision(syslog) {}

HikVisionNet::~HikVisionNet() {
	if (!NET_DVR_Cleanup()) {
		this->report_warning("cleanup");
	}
}

void HikVisionNet::initialize() {
	if (!NET_DVR_Init()) {
		this->report_error("initialize");
	}
}
