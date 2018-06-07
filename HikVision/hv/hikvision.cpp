#include "hv/hikvision.hpp"

#include "string.hpp"
#include "usr/share/include/HCNetSDK.h"

using namespace WarGrey::SCADA;

/*************************************************************************************************/
HikVision::HikVision(Syslog* syslog) : logger(syslog) {
	if (this->logger == nullptr) {
		this->logger = make_system_logger("HikVision");
	}

	this->logger->reference();
}

HikVision::~HikVision() {
	this->logger->destroy();
}

Syslog* HikVision::get_logger() {
	return this->logger;
}

void HikVision::report_error() {
	this->log(Log::Error);
}

void HikVision::report_error(const std::string& msg_prefix) {
	this->log(msg_prefix, Log::Error);
}

void HikVision::report_error(const char* format, ...) {
	VSNPRINT(message, format);
	this->log(message, Log::Error);
}

bool HikVision::report_on_error(bool okay, const std::string& msg_prefix) {
	if (!okay) {
		this->report_error(msg_prefix);
	}

	return okay;
}

void HikVision::report_warning() {
	this->log(Log::Warning);
}

void HikVision::report_warning(const std::string& msg_prefix) {
	this->log(msg_prefix, Log::Warning);
}

void HikVision::report_warning(const char* format, ...) {
	VSNPRINT(message, format);
	this->log(message, Log::Warning);
}

bool HikVision::report_on_warning(bool okay, const std::string& msg_prefix) {
	if (!okay) {
		report_warning(msg_prefix);
	}

	return okay;
}

void HikVision::log(Log level) {
	this->get_logger()->log_message(level, L"%S", NET_DVR_GetErrorMsg());
}

void HikVision::log(const std::string& message_prefix, Log level) {
	this->get_logger()->log_message(level, L"%S: %S", message_prefix.c_str(), NET_DVR_GetErrorMsg());
}
