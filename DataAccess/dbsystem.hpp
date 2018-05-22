#pragma once

#include "syslog.hpp"
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

	private:
		WarGrey::SCADA::Syslog* logger;
	};
}
