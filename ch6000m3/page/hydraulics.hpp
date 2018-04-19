#pragma once

#include "planet.hpp"
#include "mrit.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class HydraulicSystem : public WarGrey::SCADA::Planet {
	public:
		~HydraulicSystem() noexcept;
		HydraulicSystem(WarGrey::SCADA::IMRMaster* plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	public:
		void on_tap(IGraphlet* g, float x, float y, bool shifted, bool ctrled) override;

	private:
		WarGrey::SCADA::IMRMaster* device;
		WarGrey::SCADA::IMRConfirmation* console;

	private: // never deletes these graphlets mannually
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;

	private:
		float gridsize;
	};
}
