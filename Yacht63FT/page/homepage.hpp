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

	private:
		WarGrey::SCADA::IMRConfirmation* console;
	};
}
