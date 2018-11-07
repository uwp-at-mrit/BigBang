#pragma once

#include "graphlet/device/winchlet.hpp"
#include "graphlet/device/gantrylet.hpp"

namespace WarGrey::SCADA {
	private struct WinchThreshold {
	public:
		WinchThreshold(unsigned int upper, unsigned int spool, unsigned int slack = 0U, unsigned int suction = 0U)
			: upper(upper), spool(spool), suction(suction), slack(slack) {}

	public:
		unsigned int upper;
		unsigned int spool;
		unsigned int suction;
		unsigned int slack;
	};

	// DB4, starts from 1
	static WarGrey::SCADA::WinchThreshold winch_ps_trunnion_threshold = WarGrey::SCADA::WinchThreshold(321U, 322U, 323U, 324U);
	static WarGrey::SCADA::WinchThreshold winch_ps_intermediate_threshold = WarGrey::SCADA::WinchThreshold(332U, 354U);
	static WarGrey::SCADA::WinchThreshold winch_ps_draghead_threshold = WarGrey::SCADA::WinchThreshold(361U, 362U);

	static WarGrey::SCADA::WinchThreshold winch_sb_trunnion_threshold = WarGrey::SCADA::WinchThreshold(337U, 338U, 339U, 340U);
	static WarGrey::SCADA::WinchThreshold winch_sb_intermediate_threshold = WarGrey::SCADA::WinchThreshold(348U, 386U);
	static WarGrey::SCADA::WinchThreshold winch_sb_draghead_threshold = WarGrey::SCADA::WinchThreshold(393U, 394U);

	// DB205, starts from 1

	/************************************************************************************************/

	template<class W>
	void DI_winch(W* target, const uint8* db4, WarGrey::SCADA::WinchThreshold& threshold, const uint8* db205, size_t idx205_p1) {
		target->set_remote_control(DBX(db4, idx4_p1 - 1));

		if (DBX(db4, idx4_p1 + 1)) {
			target->set_status(HydraulicPumpStatus::Broken);
		} else {
			if (DBX(db4, idx4_p1 + 0)) {
				// equivalent
				target->set_status(DBX(db205, idx205_p1 + 4), HydraulicPumpStatus::StopReady);
				target->set_status(HydraulicPumpStatus::Running);
			} else {
				target->set_status(DBX(db205, idx205_p1 - 1), HydraulicPumpStatus::Starting);
				target->set_status(DBX(db205, idx205_p1 + 0), HydraulicPumpStatus::Stopping);
				target->set_status(DBX(db205, idx205_p1 + 1), HydraulicPumpStatus::Unstartable);
				target->set_status(DBX(db205, idx205_p1 + 2), HydraulicPumpStatus::Unstoppable);

				// use HydraulicPumpStatus::Ready instead of HydraulicPumpStatus::StartReady.
				target->set_status(DBX(db205, idx205_p1 + 3), HydraulicPumpStatus::Ready);

				// the rest two are not used
				// target->set_status(DBX(db205, idx205_p1 + 5), HydraulicPumpStatus::Stopped);
				// target->set_status(DBX(db205, idx205_p1 + 6), HydraulicPumpStatus::Ready);
			}
		}
	}

	/*
	template<class W, typename Menu>
	bool winch_command_executable(W* target, Menu cmd, bool otherwise) {
		WinchStatus status = target->get_status();
		bool executable = otherwise;

		switch (cmd) {
		case Menu::Start: executable = (status == HydraulicPumpStatus::Ready); break;
		case Menu::Stop: executable = (status == HydraulicPumpStatus::Running); break;
		}

		return executable;
	}

	template<class G, typename Menu>
	bool gantry_command_executable(G* target, Menu cmd, bool otherwise) {
		GantryStatus status = target->get_status();
		bool executable = otherwise;

		switch (cmd) {
		case Menu::Start: executable = (status == HydraulicPumpStatus::Ready); break;
		case Menu::Stop: executable = (status == HydraulicPumpStatus::Running); break;
		}

		return executable;
	}
	*/
}
