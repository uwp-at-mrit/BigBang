#pragma once

#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"

namespace WarGrey::SCADA {
	// DB4, starts from 1
	static unsigned int pump_A_feedback = 49U;
	static unsigned int pump_B_feedback = 53U;
	static unsigned int pump_G_feedback = 69U;
	static unsigned int pump_H_feedback = 65U;

	static unsigned int pump_C_feedback = 57U;
	static unsigned int pump_F_feedback = 73U;
	static unsigned int pump_D_feedback = 81U;
	static unsigned int pump_E_feedback = 85U;

	static unsigned int pump_Y_feedback = 101U;
	static unsigned int pump_L_feedback = 93U;
	static unsigned int pump_M_feedback = 97U;
	static unsigned int pump_K_feedback = 89U;

	static unsigned int pump_I_feedback = 77U;
	static unsigned int pump_J_feedback = 61U;

	static unsigned int pump_ps_gate_flushing_feedback = 105U;
	static unsigned int pump_sb_gate_flushing_feedback = 109U;

	static unsigned int pump_ps_hopper_A_feedback = 9U;
	static unsigned int pump_ps_hopper_B_feedback = 13U;
	static unsigned int pump_sb_hopper_A_feedback = 33U;
	static unsigned int pump_sb_hopper_B_feedback = 37U;

	static unsigned int pump_ps_underwater_1_feedback = 513U;
	static unsigned int pump_ps_underwater_2_feedback = 517U;
	static unsigned int pump_sb_underwater_1_feedback = 529U;
	static unsigned int pump_sb_underwater_2_feedback = 533U;

	// DB205, starts from 1
	static unsigned int pump_A_status = 9U;
	static unsigned int pump_B_status = 17U;
	static unsigned int pump_G_status = 57U;
	static unsigned int pump_H_status = 65U;

	static unsigned int pump_C_status = 25U;
	static unsigned int pump_F_status = 49U;
	static unsigned int pump_D_status = 33U;
	static unsigned int pump_E_status = 41U;

	static unsigned int pump_Y_status = 73U;
	static unsigned int pump_L_status = 89U;
	static unsigned int pump_M_status = 97U;
	static unsigned int pump_K_status = 81U;

	static unsigned int pump_I_status = 113U;
	static unsigned int pump_J_status = 105U;

	static unsigned int pump_ps_gate_flushing_status = 1673U;
	static unsigned int pump_sb_gate_flushing_status = 1681U;

	static unsigned int pump_ps_hopper_A_status = 1713U;
	static unsigned int pump_ps_hopper_B_status = 1721U;
	static unsigned int pump_sb_hopper_A_status = 1729U;
	static unsigned int pump_sb_hopper_B_status = 1737U;

	static unsigned int pump_ps_underwater_1_status = 1697U;
	static unsigned int pump_ps_underwater_2_status = 1705U;
	static unsigned int pump_sb_underwater_1_status = 1745U;
	static unsigned int pump_sb_underwater_2_status = 1753U;

	/************************************************************************************************/
	template<class D>
	void DI_pump_dimension(D* target, const uint8* db4, size_t idx_p1) {
		target->set_status(DBX(db4, idx_p1) ? DimensionStatus::Highlight : DimensionStatus::Normal);
	}

	template<class H>
	void DI_hydraulic_pump(H* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		target->set_remote_control(DBX(db4, idx4_p1 - 1));

		if (DBX(db4, idx4_p1 + 1)) {
			target->set_status(HydraulicPumpStatus::Broken);
		} else {
			if (DBX(db4, idx4_p1 + 0)) {
				// equivalent
				target->set_status(DBX(db205, idx205_p1 + 4), HydraulicPumpStatus::StopReady);
				target->set_status(HydraulicPumpStatus::Running);
				// use HydraulicPumpStatus::Ready instead of HydraulicPumpStatus::StartReady.	
			} else if (DBX(db205, idx205_p1 - 1)) {
				target->set_status(HydraulicPumpStatus::Starting);
			} else if (DBX(db205, idx205_p1 + 0)) {
				target->set_status(HydraulicPumpStatus::Stopping);
			} else if (DBX(db205, idx205_p1 + 1)) {
				target->set_status(HydraulicPumpStatus::Unstartable);
			} else if (DBX(db205, idx205_p1 + 2)) {
				target->set_status(HydraulicPumpStatus::Unstoppable);
			} else if (DBX(db205, idx205_p1 + 3)) {
				target->set_status(HydraulicPumpStatus::Ready);
			}
				// the rest two are not used
				// target->set_status(DBX(db205, idx205_p1 + 5), HydraulicPumpStatus::Stopped);
				// target->set_status(DBX(db205, idx205_p1 + 6), HydraulicPumpStatus::Ready);
		}
	}

	template<class H, typename Menu>
	bool hydraulic_pump_command_executable(H* target, Menu cmd, bool otherwise) {
		HydraulicPumpStatus status = target->get_status();
		bool executable = otherwise;

		switch (cmd) {
		case Menu::Start: executable = ((status == HydraulicPumpStatus::Ready) || (status == HydraulicPumpStatus::Unstartable)); break;
		}

		return executable;
	}

	/************************************************************************************************/
	template<class F>
	void DI_gate_flushing_pump(F* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1) {
		DI_hydraulic_pump(target, db4, idx4_p1, db205, idx205_p1);
	}

	template<class H, typename Menu>
	bool gate_flushing_pump_command_executable(H* target, Menu cmd, bool otherwise) {
		return hydraulic_pump_command_executable(target, cmd, otherwise);
	}

	/************************************************************************************************/
	template<class S>
	void DI_sealed_water_pump(S* target, bool underwater, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1) {
		bool ready = false;
		
		if (underwater) {
			target->set_remote_control(DBX(db4, idx_p1 - 1));
			ready = DBX(db4, idx_p1 + 0);
		} else {
			target->set_remote_control(DBX(db4, idx_p1 + 0));
			ready = DBX(db4, idx_p1 - 1);
		}

		if (ready) {
			target->set_status(HydraulicPumpStatus::Ready);
		} else if (DBX(db4, idx_p1 + 1)) {
			target->set_status(HydraulicPumpStatus::Running);
		} else if (DBX(db4, idx_p1 + 2)) {
			target->set_status(HydraulicPumpStatus::Broken);
		} else {
			target->set_status(DBX(db205, idx205_p1 - 1), HydraulicPumpStatus::Starting);
			target->set_status(DBX(db205, idx205_p1 + 0), HydraulicPumpStatus::Stopping);
			//target->set_status(DBX(db205, idx205_p1 + 1), HydraulicPumpStatus::Reset);
			target->set_status(DBX(db205, idx205_p1 + 2), HydraulicPumpStatus::Unstartable);
			target->set_status(DBX(db205, idx205_p1 + 3), HydraulicPumpStatus::Unstoppable);

			// the rest 3 are implied or not used
			//target->set_status(DBX(db205, idx205_p1 + 4), HydraulicPumpStatus::StartReady);
			//target->set_status(DBX(db205, idx205_p1 + 5), HydraulicPumpStatus::StopReady);
			//target->set_status(DBX(db205, idx205_p1 + 6), HydraulicPumpStatus::Stopped);
		}
	}

	template<class S, typename Menu>
	bool sealed_water_pump_command_executable(S* target, Menu cmd, bool otherwise) {
		return hydraulic_pump_command_executable(target, cmd, otherwise);
	}
}
