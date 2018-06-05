#pragma once

#include "syslog.hpp"

namespace WarGrey::SCADA {
	private class HikVisionPlayCtrl {
	public:
		virtual ~HikVisionPlayCtrl() noexcept;

		HikVisionPlayCtrl(WarGrey::SCADA::Syslog* syslog = nullptr);

	private:
		WarGrey::SCADA::Syslog* logger;
	};
}
