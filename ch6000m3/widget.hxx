#pragma once

#include "universe.hxx"
#include "plc.hpp"

namespace WarGrey::SCADA {
	private ref class UniverseWidget : public UniverseDisplay {
	internal:
		UniverseWidget(UniverseDisplay^ master, PLCMaster* plc);

	protected:
		void construct(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason) override;

	private:
		UniverseDisplay^ master;
		PLCMaster* plc;
	};
}
