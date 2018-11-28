#pragma once

#include "plc.hpp"

namespace WarGrey::SCADA {
	private enum class OverflowAction { Up, Down, Stop, Auto, _ };
	private enum class TankHeaterAction { Start, Stop, Reset, Auto, _ };

	Windows::UI::Xaml::Controls::MenuFlyout^ make_overflow_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_tank_heater_menu(WarGrey::SCADA::PLCMaster* plc);
}
