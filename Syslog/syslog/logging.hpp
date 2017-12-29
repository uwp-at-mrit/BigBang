#pragma once

#include "object.hpp"

namespace WarGrey::SCADA {
	private enum SyslogLevel { Debug, Info, Notice, Warning, Error, Critical, Alert, Panic, None };

	private class ISyslogData abstract : public WarGrey::SCADA::SharedObject {};

	private class ISyslogReceiver abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual ~ISyslogReceiver() noexcept {};
		ISyslogReceiver(WarGrey::SCADA::SyslogLevel level, Platform::String^ topic = "") : level(level), topic(topic) {};

	public:
		void log_message(WarGrey::SCADA::SyslogLevel level, Platform::String^ message,
			WarGrey::SCADA::ISyslogData* data, Platform::String^ topic);

	protected:
		virtual void on_log_message(
			WarGrey::SCADA::SyslogLevel level,
			Platform::String^ message,
			WarGrey::SCADA::ISyslogData* data,
			Platform::String^ topic) = 0;

	private:
		WarGrey::SCADA::SyslogLevel level;
		Platform::String^ topic;
	};

	private class ISyslog abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual ~ISyslog() noexcept;
		ISyslog(WarGrey::SCADA::SyslogLevel level, Platform::String^ topic = "", WarGrey::SCADA::ISyslog* parent = nullptr);

	private:
		WarGrey::SCADA::SyslogLevel level;
		Platform::String^ topic;
		ISyslog* parent;
	};
}
