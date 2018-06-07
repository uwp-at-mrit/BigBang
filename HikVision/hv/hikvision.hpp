#pragma once

#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class HikVision {
	public:
		virtual ~HikVision() noexcept;

		HikVision(WarGrey::SCADA::Syslog* syslog = nullptr);

	public:
		WarGrey::SCADA::Syslog* get_logger();

	public:
		void report_error();
		void report_error(const std::string& msg_prefix);
		void report_error(const char* format, ...);

		void report_warning();
		void report_warning(const std::string& msg_prefix);
		void report_warning(const char* format, ...);

	protected:
		bool report_on_error(bool okay, const std::string& msg_prefix);
		bool report_on_warning(bool okay, const std::string& msg_prefix);

	private:
		void log(WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Error);
		void log(const std::string& msg_prefix, WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Error);

	private:
		WarGrey::SCADA::Syslog* logger;
	};
}
