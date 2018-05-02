#pragma once

#include "planet.hpp"
#include "mrit.hpp"

namespace WarGrey::SCADA {
	private interface class INavigatorAction {
		void on_navigate();
	};

	private enum class Yacht {
		HomePage, Generator, Propeller, Diagram,
		AirConditioning, Illuminating, Inferno, Alert,
		Video, _
	};

	private class Navigatorbar : public WarGrey::SCADA::Planet {
	public:
		~Navigatorbar() noexcept;
		Navigatorbar(WarGrey::SCADA::INavigatorAction^ action);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) override;

	private:
		WarGrey::SCADA::INavigatorAction^ action;
	};
}
