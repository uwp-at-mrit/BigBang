#pragma once

#include "planet.hpp"

namespace WarGrey::SCADA {
	private class VisitorSpace : public WarGrey::SCADA::Planet {
	public:
		virtual ~VisitorSpace() noexcept;
		VisitorSpace();

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		
	public:
		void on_tap(WarGrey::SCADA::IGraphlet* g, float x, float y) override;
	};
}
