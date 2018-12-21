#pragma once

#include "universe.hxx"
#include "plc.hpp"

namespace WarGrey::SCADA {
	private ref class SettingsWidget : public UniverseDisplay {
	internal:
		SettingsWidget(Syslog* logger, PLCMaster* plc);

	protected:
		void construct() override;

	private:
		PLCMaster* plc;
	};
}
