#pragma once

#include "planet.hpp"
#include "graphlet/planetlet.hpp"

namespace WarGrey::SCADA {
	private class VisitorSpace : public WarGrey::SCADA::Planet {
	public:
		virtual ~VisitorSpace() noexcept;
		VisitorSpace();

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void reflow(float width, float height) override;

	private: // never deletes these graphlets manually
		WarGrey::SCADA::Planetlet* space;
	};
}
