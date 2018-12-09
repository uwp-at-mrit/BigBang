#pragma once

#include "plc.hpp"

namespace WarGrey::SCADA {
	private enum class DredgesGroup { PSGantries, SBGantries, _ };
	private enum class DredgesPosition { psTrunnion, psIntermediate, psDragHead, sbTrunnion, sbIntermediate, sbDragHead, _};

	private enum class WinchAction { Up, Down, Stop, HighSpeed, _ };
	private enum class GantryAction { WindOut, WindUp, Stop, _ };
	private enum class WaveCompensatorAction { Charge, Discharge, Stop, _ };
	private enum class DragVisorAction { Up, Down, Stop, _ };

	private enum class SuctionCommand { Inflate, Deflate, _ };
	private enum class DragVisorCommand { CResistance, _ };
	private enum class GantryCommand { VirtualUp, VirtualOut, _ };
	private enum class LMODCommand { PSALMO, SBALMO, _ };

	Windows::UI::Xaml::Controls::MenuFlyout^ make_winch_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_gantry_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_wave_compensator_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_drag_visor_menu(WarGrey::SCADA::PLCMaster* plc);
	Windows::UI::Xaml::Controls::MenuFlyout^ make_gantry_group_menu(WarGrey::SCADA::DredgesGroup group, WarGrey::SCADA::PLCMaster* plc);

	// DB300, starts from 1
	static unsigned int drag_ps_visor_constant_resistance_command = 659U;
	static unsigned int drag_sb_visor_constant_resistance_command = 660U;

	static unsigned int ctension_ps_buttons = 659U;
	static unsigned int ctension_sb_buttons = 660U;

	uint16 DO_winch_override_command(WarGrey::SCADA::DredgesPosition id);
	uint16 DO_suction_command(WarGrey::SCADA::SuctionCommand cmd, bool ps);
	uint16 DO_LMOD_command(WarGrey::SCADA::LMODCommand cmd);
	uint16 DO_gantry_virtual_action_command(WarGrey::SCADA::DredgesPosition gid, WarGrey::SCADA::GantryCommand cmd);
}
