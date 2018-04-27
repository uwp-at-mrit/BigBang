#pragma once

#include "planet.hpp"
#include "mrit.hpp"

namespace WarGrey::SCADA {
	private class Statusbar : public WarGrey::SCADA::Planet {
	public:
		~Statusbar() noexcept;
		Statusbar();

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) override;

	private:
		WarGrey::SCADA::IMRConfirmation* dashboard;
	};
}
