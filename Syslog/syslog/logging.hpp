#pragma once

#include <list>

#include "object.hpp"

namespace WarGrey::SCADA {
	private enum class Log { Debug, Info, Notice, Warning, Error, Critical, Alarm, Panic, _ };

	private struct SyslogMetainfo {
		Platform::String^ timestamp;
	};

	private class ISyslogReceiver abstract : public WarGrey::SCADA::SharedObject {
	public:
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

	protected:
		~ISyslogReceiver() noexcept {}

	private:
		WarGrey::SCADA::Log level;
		Platform::String^ topic;
	};

	private class Syslog final : public WarGrey::SCADA::SharedObject {
	public:
		Syslog(WarGrey::SCADA::Log level, Platform::String^ topic = "", WarGrey::SCADA::Syslog* parent = nullptr);

	public:
		Platform::String^ get_name();

	public:
		void push_log_receiver(ISyslogReceiver* receiver);
		
		void log_message(WarGrey::SCADA::Log level, Platform::String^ message);
		void log_message(WarGrey::SCADA::Log level, const wchar_t* msgfmt, ...);
		
		void log_message(Platform::String^ alt_topic, WarGrey::SCADA::Log level, Platform::String^ message);
		void log_message(Platform::String^ alt_topic, WarGrey::SCADA::Log level, const wchar_t* msgfmt, ...);

	protected:
		~Syslog() noexcept;

	private:
		void do_log_message(WarGrey::SCADA::Log level, Platform::String^ message, Platform::String^ alt_topic, bool prefix);

	private:
		WarGrey::SCADA::Log level;
		Platform::String^ topic;
		Syslog* parent;

	private:
		std::list<WarGrey::SCADA::ISyslogReceiver*> receivers;
	};
}
