#pragma once

#include "hv/hikvision.hpp"

namespace WarGrey::SCADA {
	private class HikVisionNet : public WarGrey::SCADA::HikVision {
	public:
		virtual ~HikVisionNet() noexcept;

		HikVisionNet(WarGrey::SCADA::Syslog* syslog = nullptr);

	public:
		void initialize();
	};
}
