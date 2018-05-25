#pragma once

#include "syslog.hpp"
#include "vsql.hpp"

namespace WarGrey::SCADA {
	private enum class DBMS { SQLite3 };

	private class IPreparedStatement {
	};

	private class IDBSystem {
	public:
		virtual ~IDBSystem() noexcept;

		IDBSystem(WarGrey::SCADA::Syslog* logger);

	public:
		WarGrey::SCADA::Syslog* get_logger();

	public:
		virtual WarGrey::SCADA::DBMS system() = 0;
		virtual const wchar_t* get_last_error_message() = 0;
		virtual WarGrey::SCADA::IVirtualSQL* make_sql_factory(WarGrey::SCADA::TableColumnInfo* columns, size_t count) = 0;
		virtual void exec(Platform::String^ stmt) = 0;

	public:
		void exec(const wchar_t* sql, ...);

	public:
		void report_error(Platform::String^ msg_prefix = nullptr);
		void report_error(const wchar_t* format, ...);

		void report_warning(Platform::String^ msg_prefix = nullptr);
		void report_warning(const wchar_t* format, ...);

	private:
		void log(Platform::String^ msg_prefix = nullptr, WarGrey::SCADA::Log level = WarGrey::SCADA::Log::Error);

	private:
		WarGrey::SCADA::Syslog* logger;
	};
}
