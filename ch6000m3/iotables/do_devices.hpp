#pragma once

#include "plc.hpp"
#include "graphlet/primitive.hpp"

namespace WarGrey::SCADA {
	private enum class OverflowAction { Up, Down, Stop, Auto, _ };
	private enum class TankHeaterAction { Start, Stop, Cancel, Auto, _ };

	Windows::UI::Xaml::Controls::MenuFlyout^ make_overflow_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_tank_heater_menu(WarGrey::SCADA::PLCMaster* plc);
}
