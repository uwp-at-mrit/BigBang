#pragma once

#include "plc.hpp"

namespace WarGrey::SCADA {
	// DB300, starts from 1
	static unsigned int backoil_pressure_override_command = 816U;

	/************************************************************************************************/
	private enum class OverflowAction { Up, Down, Stop, Auto, _ };
	private enum class TankHeaterAction { Start, Stop, Reset, Auto, _ };
	private enum class AnchorWinchAction { Up, Down, Stop, _ };

	Windows::UI::Xaml::Controls::MenuFlyout^ make_overflow_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_tank_heater_menu(WarGrey::SCADA::PLCMaster* plc);
}
