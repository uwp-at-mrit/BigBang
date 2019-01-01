#pragma once

#include "universe.hxx"
#include "plc.hpp"

namespace WarGrey::SCADA {
	private ref class UniverseWidget : public UniverseDisplay {
	internal:
		UniverseWidget(UniverseDisplay^ master, PLCMaster* plc);

	protected:
		void construct() override;

	private:
		UniverseDisplay^ master;
		PLCMaster* plc;
	};
}
