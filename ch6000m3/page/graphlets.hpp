#pragma once

#include "planet.hpp"
#include "modbus.hpp"

#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

namespace WarGrey::SCADA {
	private class Graphlets : public WarGrey::SCADA::Planet {
	public:
		~Graphlets() noexcept;
		Graphlets();

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	private: // never deletes these snips mannually	
		WarGrey::SCADA::Statusbarlet* statusbar;
		WarGrey::SCADA::Statuslinelet* statusline;
	};
}
