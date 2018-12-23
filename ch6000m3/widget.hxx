#pragma once

#include "universe.hxx"
#include "plc.hpp"

namespace WarGrey::SCADA {
	private ref class UniverseWidget : public UniverseDisplay {
	internal:
		UniverseWidget(Syslog* logger, PLCMaster* plc);

	protected:
		void construct() override;

	private:
		PLCMaster* plc;
	};
}
