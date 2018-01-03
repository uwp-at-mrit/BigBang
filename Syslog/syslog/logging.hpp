#pragma once

#include <list>

#include "object.hpp"

namespace WarGrey::SCADA {
	private enum class Log { Debug, Info, Notice, Warning, Error, Critical, Alert, Panic, None };

	private struct SyslogMetainfo {
		Platform::String^ timestamp;
	};

	private class ISyslogReceiver abstract : public WarGrey::SCADA::SharedObject {
	public:
		virtual ~ISyslogReceiver() noexcept {};
		ISyslogReceiver(WarGrey::SCADA::Log level, Platform::String^ topic = "")
			: level(level), topic(topic) {};

	public:
		void log_message(WarGrey::SCADA::Log level, Platform::String^ message,
			WarGrey::SCADA::SyslogMetainfo& data, Platform::String^ topic);

	protected:
		virtual void on_log_message(
			WarGrey::SCADA::Log level,
			Platform::String^ message,
			WarGrey::SCADA::SyslogMetainfo& data,
			Platform::String^ topic) = 0;

	private:
		WarGrey::SCADA::Log level;
		Platform::String^ topic;
	};

	private class Syslog : public WarGrey::SCADA::SharedObject {
	public:
		virtual ~Syslog() noexcept;
		Syslog(WarGrey::SCADA::Log level, Platform::String^ topic = "", WarGrey::SCADA::Syslog* parent = nullptr);

	public:
		void append_log_receiver(ISyslogReceiver* receiver);
		
		void log_message(WarGrey::SCADA::Log level, Platform::String^ message, bool prefix = true);
		void log_message(WarGrey::SCADA::Log level, const wchar_t* msgfmt, ...);
		
		void log_message(Platform::String^ alt_topic, WarGrey::SCADA::Log level, Platform::String^ message, bool prefix = true);
		void log_message(Platform::String^ alt_topic, WarGrey::SCADA::Log level, const wchar_t* msgfmt, ...);

	private:
		WarGrey::SCADA::Log level;
		Platform::String^ topic;
		Syslog* parent;

	private:
		std::list<WarGrey::SCADA::ISyslogReceiver*>* receivers;
	};
}
