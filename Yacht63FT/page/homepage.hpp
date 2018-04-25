#pragma once

#include "planet.hpp"
#include "mrit.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class Homepage : public WarGrey::SCADA::Planet {
	public:
		~Homepage() noexcept;
		Homepage(WarGrey::SCADA::IMRMaster* plc);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		
	public:
		void on_tap(IGraphlet* g, float x, float y, bool shifted, bool ctrled) override;

	private:
		WarGrey::SCADA::IMRMaster* device;
		WarGrey::SCADA::IMRConfirmation* console;

	private:
		float gridsize;
	};
}
