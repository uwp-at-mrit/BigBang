#pragma once

#include "planet.hpp"
#include "mrit.hpp"

namespace WarGrey::SCADA {
	private class Navigatorbar : public WarGrey::SCADA::Planet {
	public:
		~Navigatorbar() noexcept;
		Navigatorbar();

	public:
		void set_workspace(UniverseDisplay^ master_display);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) override;

	private:
		WarGrey::SCADA::UniverseDisplay^ master;
	};
}
