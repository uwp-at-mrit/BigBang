#pragma once

#include "planet.hpp"
#include "mrit.hpp"

namespace WarGrey::SCADA {
	private enum class Yacht {
		HomePage, Propulsion, Generator, Propeller,
		Gauge, AirConditioner, Operation, Event,
		_
	};

	private interface class INavigatorAction {
		void on_navigate(WarGrey::SCADA::Yacht page);
	};

	private class Navigatorbar : public WarGrey::SCADA::Planet {
	public:
		~Navigatorbar() noexcept;
		Navigatorbar(WarGrey::SCADA::IMRMaster* device, WarGrey::SCADA::INavigatorAction^ action);

	public:
		void load(Microsoft::Graphics::Canvas::UI::CanvasCreateResourcesReason reason, float width, float height) override;
		void on_tap(WarGrey::SCADA::IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) override;

	public:
		void on_navigated_to(WarGrey::SCADA::Yacht page);

	private:
		WarGrey::SCADA::IMRMaster* device;
		WarGrey::SCADA::INavigatorAction^ action;
	};
}
