#pragma once

#include "graphlet/textlet.hpp"
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

	static unsigned int pump_I_feedback = 61U;
	static unsigned int pump_J_feedback = 77U;

	static unsigned int pump_ps_gate_flushing_feedback = 105U;
	static unsigned int pump_sb_gate_flushing_feedback = 109U;

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

	static unsigned int pump_I_status = 105U;
	static unsigned int pump_J_status = 113U;

	static unsigned int pump_ps_gate_flushing_status = 1673U;
	static unsigned int pump_sb_gate_flushing_status = 1681U;

	/************************************************************************************************/
	void DI_pump_dimension(WarGrey::SCADA::Dimensionlet* target, const uint8* db4, size_t idx_p1);
	void DI_hydraulic_pump(WarGrey::SCADA::HydraulicPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1);

	/************************************************************************************************/
	void DI_gate_flushing_pump(WarGrey::SCADA::HydraulicPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1);
}
