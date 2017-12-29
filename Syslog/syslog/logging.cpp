#include "logging.hpp"

using namespace WarGrey::SCADA;

void ISyslogReceiver::log_message(SyslogLevel level, Platform::String^ message, ISyslogData* data, Platform::String^ topic) {
	if (level > this->level) {
		if ((this->topic == nullptr) || (this->topic->Equals(topic))) {
			this->on_log_message(level, message, data, topic);
		}
	}
}

ISyslog::ISyslog(SyslogLevel level, Platform::String^ topic, ISyslog* parent) : level(level), topic(topic) {
	if (parent != nullptr) {
		this->parent = parent;
		this->parent->reference();
	}
}

ISyslog::~ISyslog() {
	if (parent != nullptr) {
		this->parent->destroy();
	}
}
