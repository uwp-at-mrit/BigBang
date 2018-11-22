#pragma once

#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"
#include "graphlet/dashboard/alarmlet.hpp"

namespace WarGrey::SCADA {
#define DI_hopper_type(db4, idx_p1) DBX(db4, idx_p1 + 0U)
#define DI_underwater_type(db4, idx_p1) DBX(db4, idx_p1 + 1U)

	// DB4, starts from 1
	static unsigned int ps_hopper_pump_feedback = 1U;
	static unsigned int sb_hopper_pump_feedback = 25U;

	static unsigned int pump_ps_hopper_A_feedback = 9U;
	static unsigned int pump_ps_hopper_B_feedback = 13U;
	static unsigned int pump_sb_hopper_A_feedback = 33U;
	static unsigned int pump_sb_hopper_B_feedback = 37U;

	static unsigned int pump_ps_underwater_1_feedback = 513U;
	static unsigned int pump_ps_underwater_2_feedback = 517U;
	static unsigned int pump_sb_underwater_1_feedback = 529U;
	static unsigned int pump_sb_underwater_2_feedback = 533U;

	static unsigned int ps_hopper_lubricating_unit_feedback = 481U;
	static unsigned int ps_hopper_lubricating_unit_alarms = 486U;

	static unsigned int ps_hopper_gearbox_master_feedback = 521U;
	static unsigned int ps_hopper_gearbox_spare_feedback = 524U;
	static unsigned int ps_hopper_gearbox_alarms = 527U;

	static unsigned int sb_hopper_lubricating_unit_feedback = 497U;
	static unsigned int sb_hopper_lubricating_unit_alarms = 502U;
	
	static unsigned int sb_hopper_gearbox_master_feedback = 537U;
	static unsigned int sb_hopper_gearbox_spare_feedback = 540U;
	static unsigned int sb_hopper_gearbox_alarms = 543U;
	
	// DB205, starts from 1
	static unsigned int ps_hopper_pump_details = 857U;
	static unsigned int ps_underwater_pump_details = 825U;
	static unsigned int ps_hopper_gearbox_master_details = 1209U;
	static unsigned int ps_hopper_gearbox_spare_details = 1217U;

	static unsigned int sb_hopper_pump_details = 873U;
	static unsigned int sb_underwater_pump_details = 841U;
	static unsigned int sb_hopper_gearbox_master_details = 1225U;
	static unsigned int sb_hopper_gearbox_spare_details = 1233U;

	static unsigned int pump_ps_hopper_A_status = 1713U;
	static unsigned int pump_ps_hopper_B_status = 1721U;
	static unsigned int pump_sb_hopper_A_status = 1729U;
	static unsigned int pump_sb_hopper_B_status = 1737U;

	static unsigned int pump_ps_underwater_1_status = 1697U;
	static unsigned int pump_ps_underwater_2_status = 1705U;
	static unsigned int pump_sb_underwater_1_status = 1745U;
	static unsigned int pump_sb_underwater_2_status = 1753U;

	/************************************************************************************************/
	void DI_hopper_pumps(WarGrey::SCADA::HopperPumplet* t1, WarGrey::SCADA::HopperPumplet* t2, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_1_p1, size_t idx205_2_p1);
	void DI_hopper_pump(WarGrey::SCADA::HopperPumplet* target, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1);

	/************************************************************************************************/
	void DI_hopper_gland_pump(WarGrey::SCADA::HydraulicPumplet* target, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1);
	void DI_underwater_gland_pump(WarGrey::SCADA::HydraulicPumplet* target, const uint8* db4, size_t idx_p1, const uint8* db205, size_t idx205_p1);

	template<class S, typename Menu>
	bool gland_pump_command_executable(S* target, Menu cmd, bool otherwise) {
		return hydraulic_pump_command_executable(target, cmd, otherwise);
	}

	/************************************************************************************************/
	void DI_hopper_pump_lubricating_unit(WarGrey::SCADA::HydraulicPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1);
	void DI_hopper_pump_gearbox(WarGrey::SCADA::HydraulicPumplet* target, const uint8* db4, size_t idx4_p1, const uint8* db205, size_t idx205_p1);
	void DI_hopper_lubricating_unit_alarm(WarGrey::SCADA::Alarmlet* target, const uint8* db4, unsigned int idx_p1);
}
