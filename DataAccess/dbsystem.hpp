#pragma once

#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class IPreparedStatement {
	};

	private struct TableColumnInfo {
		Platform::String^ name;
		Platform::String^ type;
		bool notnull;
		bool unique;
		bool primary;
		Platform::String^ dflt_value;
	};

	private class IDBSystem {
	public:
		virtual ~IDBSystem() noexcept;

		IDBSystem(WarGrey::SCADA::Syslog* logger);

	public:
		WarGrey::SCADA::Syslog* get_logger();

	public:
		virtual Platform::String^ get_name() = 0;

	private:
		WarGrey::SCADA::Syslog* logger;
	};
}
