#pragma once

#include "planet.hpp"

namespace WarGrey::SCADA {
	private class CyberSpace : public WarGrey::SCADA::Planet {
	public:
		~CyberSpace() noexcept;
		CyberSpace();

	public:
		void on_tap(WarGrey::SCADA::IGraphlet* g, float x, float y, bool shifted, bool controled) override;
		void on_right_tap(WarGrey::SCADA::IGraphlet* g, float x, float y, bool shifted, bool controled) override;
	};
}
