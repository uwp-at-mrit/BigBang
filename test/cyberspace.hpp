#pragma once

#include "planet.hpp"

namespace WarGrey::SCADA {
	private class CyberSpace : public WarGrey::SCADA::Planet {
	public:
		~CyberSpace() noexcept;
		CyberSpace();

	public:
		void on_tap(WarGrey::SCADA::IGraphlet* g, float x, float y) override;
	};
}
