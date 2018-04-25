#pragma once

#include "planet.hpp"
#include "mrit.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class Homepage : public WarGrey::SCADA::Planet {
	public:
		~Homepage() noexcept;
		Homepage();

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) override;

	private:
		WarGrey::SCADA::IMRConfirmation* console;
	};
}
