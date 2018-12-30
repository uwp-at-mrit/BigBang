#pragma once

namespace WarGrey::SCADA {
	private enum class GantryWinchTrunnionSettings {
		_Gantry, GantryFlow, PushFlow, PullFlow, GantryPressure,
		_Winch, WinchFlow, PushUpFlow, PushOutFlow, PullUpFlow, PullOutFlow, WinchPressure, RemoteFlow, RemotePressure,
		_
	};

	private enum class GantryWinchIntermediateSettings {
		_Gantry, GantryFlow, PushFlow, PullFlow, GantryPressure,
		_Winch, WinchFlow, PushUpFlow, PushOutFlow, PullUpFlow, PullOutFlow, DragOutFlow, DragUpFlow, SPWCUpFlow, SPWCOutFlow, WinchPressure,
		_
	};

	private enum class GantryWinchDragHeadSettings {
		_Gantry, GantryFlow, PushFlow, PullFlow, GantryPressure,
		_Winch, WinchFlow, PushUpFlow, PushOutFlow, PullUpFlow, PullOutFlow,
		DragOutFlow, DragUpFlow, SPWCFastUpFlow, SPWCFastOutFlow, SPWCSlowUpFlow, SPWCSlowOutFlow, WinchPressure,
		_
	};

	unsigned int AO_gantry_winch_trunnion_settings(GantryWinchTrunnionSettings setting, bool ps);
	unsigned int AO_gantry_winch_intermediate_settings(GantryWinchIntermediateSettings setting, bool ps);
	unsigned int AO_gantry_winch_draghead_settings(GantryWinchDragHeadSettings setting, bool ps);
}
