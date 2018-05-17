#pragma once

#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class DBSystem {
	public:
		virtual ~DBSystem() noexcept;

		DBSystem(WarGrey::SCADA::Syslog* logger);

	public:
		WarGrey::SCADA::Syslog* get_logger();

	public:
		virtual Platform::String^ get_name() = 0;

	private:
		WarGrey::SCADA::Syslog* logger;
	};
}
