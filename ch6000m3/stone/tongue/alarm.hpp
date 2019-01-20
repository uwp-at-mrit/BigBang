#pragma once

#include "tongue.hpp"

namespace WarGrey::SCADA {
    private class Alarm : public WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm> {
        friend class WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>;
    public:
        static Platform::String^ type() { return "alarm"; }
        static unsigned int min_index() { return 39U; }
        static unsigned int max_index() { return 3927U; }

    public:
        static WarGrey::SCADA::Alarm* PSHopperMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(39U); } // PS Hopper/Underwater Converter Mainenance
        static WarGrey::SCADA::Alarm* PSGlandPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(43U); } // PS Master Gland Pump Broken
        static WarGrey::SCADA::Alarm* PSGlandPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(47U); } // PS Spare Gland Pump Broken
        static WarGrey::SCADA::Alarm* PSWaterPumpAlert() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(51U); } // PS Water Pump Alert
        static WarGrey::SCADA::Alarm* PSWaterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(52U); } // PS Water Pump Broken
        static WarGrey::SCADA::Alarm* PSWaterPumpMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(54U); } // PS Water Pump Maintenance
        static WarGrey::SCADA::Alarm* PSWaterPumpEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(55U); } // PS Water Pump Emergence
        static WarGrey::SCADA::Alarm* SBHopperMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(63U); } // SB Hopper/Underwater Converter Mainenance
        static WarGrey::SCADA::Alarm* SBGlandPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(67U); } // SB Master Gland Pump Broken
        static WarGrey::SCADA::Alarm* SBGlandPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(71U); } // SB Spare Gland Pump Broken
        static WarGrey::SCADA::Alarm* SBWaterPumpAlert() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(75U); } // SB Water Pump Alert
        static WarGrey::SCADA::Alarm* SBWaterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(76U); } // SB Water Pump Broken
        static WarGrey::SCADA::Alarm* SBWaterPumpMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(79U); } // SB Water Pump Emergence
        static WarGrey::SCADA::Alarm* PSDHPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(82U); } // PS Drag Head Pump A Broken
        static WarGrey::SCADA::Alarm* PSDIPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(86U); } // PS Drag Intermediate Pump B Broken
        static WarGrey::SCADA::Alarm* PSDTPumpCBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(90U); } // PS Drag Trunnion Pump C Broken
        static WarGrey::SCADA::Alarm* SBDVPumpIBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(94U); } // SB Drag Visor Pump I Broken
        static WarGrey::SCADA::Alarm* SBDHPumpHBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(98U); } // SB Drag Head Pump H Broken
        static WarGrey::SCADA::Alarm* SBDIPumpGBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(102U); } // SB Drag Intermediate Pump G Broken
        static WarGrey::SCADA::Alarm* SBDTPumpFBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(106U); } // SB Drag Trunnion Pump F Broken
        static WarGrey::SCADA::Alarm* SBDVPumpJBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(110U); } // SB Drag Visor Pump J Broken
        static WarGrey::SCADA::Alarm* DLBVPumpDBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(114U); } // Door Locker/Butterfly Valve Pump D Broken
        static WarGrey::SCADA::Alarm* DLBVPumpEBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(118U); } // Door Locker/Butterfly Valve Pump E Broken
        static WarGrey::SCADA::Alarm* CoolanPumpKBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(122U); } // Coolant Pump K Broken
        static WarGrey::SCADA::Alarm* MotorFlushingPumpLBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(126U); } // Motor Flushing Pump L Broken
        static WarGrey::SCADA::Alarm* SparePumpMBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(130U); } // Spare Pump M Broken
        static WarGrey::SCADA::Alarm* EmergencePumpYBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(134U); } // Emergence Pump Y Broken
        static WarGrey::SCADA::Alarm* PSGVFlushingPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(138U); } // PS Gate Valve Flushing Pump Broken
        static WarGrey::SCADA::Alarm* SBGVFlushingPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(142U); } // SB Gate Valve Flushing Pump Broken
        static WarGrey::SCADA::Alarm* MasterTankLS2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(150U); } // Master Tank Level Ultra Low
        static WarGrey::SCADA::Alarm* VisorTankLS2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(162U); } // Drag Visor Tank Level Ultra Low
        static WarGrey::SCADA::Alarm* PumpA2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(176U); } // Pump A Replace C
        static WarGrey::SCADA::Alarm* PumpC2A() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(177U); } // Pump C Replace A
        static WarGrey::SCADA::Alarm* PumpB2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(178U); } // Pump B Replace C
        static WarGrey::SCADA::Alarm* PumpC2B() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(179U); } // Pump C Replace B
        static WarGrey::SCADA::Alarm* PumpF2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(180U); } // Pump F Replace C
        static WarGrey::SCADA::Alarm* PumpC2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(181U); } // Pump C Replace F
        static WarGrey::SCADA::Alarm* PumpH2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(182U); } // Pump H Replace F
        static WarGrey::SCADA::Alarm* PumpF2H() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(183U); } // Pump F Replace H
        static WarGrey::SCADA::Alarm* PumpG2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(184U); } // Pump G Replace F
        static WarGrey::SCADA::Alarm* PumpF2G() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(185U); } // Pump F Replace G
        static WarGrey::SCADA::Alarm* PumpI2J() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(186U); } // Pump I Replace J
        static WarGrey::SCADA::Alarm* PumpJ2I() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(187U); } // Pump J Replace I
        static WarGrey::SCADA::Alarm* ShoreWinchEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(195U); } // Shore Dischange Winch Emergence
        static WarGrey::SCADA::Alarm* PSDTEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(207U); } // PS Drag Trunnion Emergence
        static WarGrey::SCADA::Alarm* PSDIEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(215U); } // PS Drag Intermediate Emergence
        static WarGrey::SCADA::Alarm* PSDHEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(223U); } // PS Drag Head Emergence
        static WarGrey::SCADA::Alarm* SBDTEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(231U); } // SB Drag Trunnion Emergence
        static WarGrey::SCADA::Alarm* SBDIEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(239U); } // SB Drag Intermediate Emergence
        static WarGrey::SCADA::Alarm* SBDHEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(247U); } // SB Drag Head Emergence
        static WarGrey::SCADA::Alarm* BargeWinchEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(255U); } // Barge Winch Emergence
        static WarGrey::SCADA::Alarm* PSHopperUnitPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(517U); } // PS Hopper Lubricating Unit Pressure Low
        static WarGrey::SCADA::Alarm* PSHopperUnitLevelLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(518U); } // PS Hopper Lubricating Unit Level Low
        static WarGrey::SCADA::Alarm* PSHopperUnitOilTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(519U); } // PS Hopper Lubricating Unit Oil Temperature High
        static WarGrey::SCADA::Alarm* PSHopperUnitWaterTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(520U); } // PS Hopper Lubricating Unit Water Temperature High
        static WarGrey::SCADA::Alarm* PSHopperUnitBearingTemperatureHigh1() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(521U); } // PS Hopper Lubricating Unit Bearing Temperature 1# High
        static WarGrey::SCADA::Alarm* PSHopperUnitBearingTemperatureHigh2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(522U); } // PS Hopper Lubricating Unit Bearing Temperature 2# High
        static WarGrey::SCADA::Alarm* PSHopperUnitBearingTemperatureHigh3() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(523U); } // PS Hopper Lubricating Unit Bearing Temperature 3# High
        static WarGrey::SCADA::Alarm* SBHopperUnitPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(533U); } // SB Hopper Lubricating Unit Pressure Low
        static WarGrey::SCADA::Alarm* SBHopperUnitLevelLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(534U); } // SB Hopper Lubricating Unit Level Low
        static WarGrey::SCADA::Alarm* SBHopperUnitOilTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(535U); } // SB Hopper Lubricating Unit Oil Temperature High
        static WarGrey::SCADA::Alarm* SBHopperUnitWaterTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(536U); } // SB Hopper Lubricating Unit Water Temperature High
        static WarGrey::SCADA::Alarm* SBHopperUnitBearingTemperatureHigh1() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(537U); } // SB Hopper Lubricating Unit Bearing Temperature 1# High
        static WarGrey::SCADA::Alarm* SBHopperUnitBearingTemperatureHigh2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(538U); } // SB Hopper Lubricating Unit Bearing Temperature 2# High
        static WarGrey::SCADA::Alarm* SBHopperUnitBearingTemperatureHigh3() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(539U); } // SB Hopper Lubricating Unit Bearing Temperature 3# High
        static WarGrey::SCADA::Alarm* PSUnderWaterGlandPump1Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(547U); } // PS Underwater Master Gland Pump 1# Broken
        static WarGrey::SCADA::Alarm* PSUnderWaterGlandPump2Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(551U); } // PS Underwater Master Gland Pump 2# Broken
        static WarGrey::SCADA::Alarm* PSHopperGearboxMasterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(554U); } // PS Hopper Gearbox Master Pump Broken
        static WarGrey::SCADA::Alarm* PSHopperGearboxSparePumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(557U); } // PS Hopper Gearbox Spare Pump Broken
        static WarGrey::SCADA::Alarm* PSHopperGearboxTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(558U); } // PS Hopper Gearbox Temperature High
        static WarGrey::SCADA::Alarm* PSHopperGearboxPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(559U); } // PS Hopper Gearbox Pressure Low
        static WarGrey::SCADA::Alarm* SBUnderWaterGlandPump1Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(563U); } // SB Underwater Master Gland Pump 1# Broken
        static WarGrey::SCADA::Alarm* SBUnderWaterGlandPump2Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(567U); } // SB Underwater Master Gland Pump 2# Broken
        static WarGrey::SCADA::Alarm* SBHopperGearboxMasterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(570U); } // SB Hopper Gearbox Master Pump Broken
        static WarGrey::SCADA::Alarm* SBHopperGearboxSparePumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(573U); } // SB Hopper Gearbox Spare Pump Broken
        static WarGrey::SCADA::Alarm* SBHopperGearboxTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(574U); } // SB Hopper Gearbox Temperature High
        static WarGrey::SCADA::Alarm* SBHopperGearboxPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(575U); } // SB Hopper Gearbox Pressure Low
        static WarGrey::SCADA::Alarm* HydraulicsStoppedBySailing() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(700U); } // Hydraulics Stopped(Sailing Console)
        static WarGrey::SCADA::Alarm* PSDIAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3050U); } // PS Drag Intermediate Angle Exceed
        static WarGrey::SCADA::Alarm* SBDIAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3051U); } // SB Drag Intermediate Angle Exceed
        static WarGrey::SCADA::Alarm* BackOilPressureFail() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3441U); } // Back Oil Pressure Less Than 3bar, All Winches are Limited
        static WarGrey::SCADA::Alarm* ET200M_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3688U); } // 11#ET200M - Bow PLC 1 Lost
        static WarGrey::SCADA::Alarm* ET200M_BowPLC2Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3689U); } // 12#ET200M - Bow PLC 2 ET200M Lost
        static WarGrey::SCADA::Alarm* ET200M_BowPLC3Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3690U); } // 13#ET200M - Bow PLC 3 ET200M Lost
        static WarGrey::SCADA::Alarm* ET200M_BowPLC4Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3691U); } // 14#ET200M - Bow PLC 4 ET200M Lost
        static WarGrey::SCADA::Alarm* ET200M_BowPLC5Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3692U); } // 15#ET200M - Bow PLC 5 ET200M Lost
        static WarGrey::SCADA::Alarm* ET200M_ConsoleDCC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3693U); } // 16#ET200M - Dredging Console DCC1 Lost
        static WarGrey::SCADA::Alarm* ET200M_ConsoleDCC2Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3694U); } // 17#ET200M - Dredging Console DCC2 Lost
        static WarGrey::SCADA::Alarm* PSDTWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3695U); } // 21# - PS Drag Trunnion Winch Lost
        static WarGrey::SCADA::Alarm* SBDTWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3696U); } // 22# - SB Drag Trunnion Winch Lost
        static WarGrey::SCADA::Alarm* PSDIWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3697U); } // 23# - PS Drag Intermediate Winch Lost
        static WarGrey::SCADA::Alarm* SBDIWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3698U); } // 24# - SB Drag Intermediate Winch Lost
        static WarGrey::SCADA::Alarm* PSDHWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3699U); } // 25# - PS Drag Head Winch Lost
        static WarGrey::SCADA::Alarm* SBDHWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3700U); } // 26# - SB Drag Head Winch Lost
        static WarGrey::SCADA::Alarm* BargeWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3701U); } // 27# - Barge Winch Lost
        static WarGrey::SCADA::Alarm* MasterCPU_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3702U); } // 2#CPU(Master) - Bow PLC 1 Lost
        static WarGrey::SCADA::Alarm* SpareCPU_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3703U); } // 3#CPU(Spare) - Bow PLC 1 Lost
        static WarGrey::SCADA::Alarm* DCSDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3704U); } // 18# DCS DP Lost
        static WarGrey::SCADA::Alarm* PSHPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3705U); } // 31# PS Hopper Pump DP Lost
        static WarGrey::SCADA::Alarm* SBHPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3706U); } // 32# SB Hopper Pump DP Lost
        static WarGrey::SCADA::Alarm* PSWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3707U); } // 33# PS Water Pump DP Lost
        static WarGrey::SCADA::Alarm* SBWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3708U); } // 34# SB Water Pump DP Lost
        static WarGrey::SCADA::Alarm* PSUWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3709U); } // 35# PS Underwater Pump DP Lost
        static WarGrey::SCADA::Alarm* SBUWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3710U); } // 36# PS Underwater Pump DP Lost
        static WarGrey::SCADA::Alarm* PLCPowerBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3711U); } // PLC Power Broken
        static WarGrey::SCADA::Alarm* TildemeterLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3713U); } // Tildemeter Lost
        static WarGrey::SCADA::Alarm* GPSLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3714U); } // GPS Lost
        static WarGrey::SCADA::Alarm* GyrocompassLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3715U); } // Gyrocompass Lost
        static WarGrey::SCADA::Alarm* PSFAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3920U); } // PS Forearm Drag Angle Exceed
        static WarGrey::SCADA::Alarm* PSBAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3921U); } // PS Backarm Drag Angle Exceed
        static WarGrey::SCADA::Alarm* PSFBAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3922U); } // PS Fore-Back Angle Exceed
        static WarGrey::SCADA::Alarm* PSDTCableExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3923U); } // PS Drag Trunnion Cable Exceed
        static WarGrey::SCADA::Alarm* SBFAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3924U); } // SB Forearm Drag Angle Exceed
        static WarGrey::SCADA::Alarm* SBBAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3925U); } // SB Backarm Drag Angle Exceed
        static WarGrey::SCADA::Alarm* SBFBAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3926U); } // SB Fore-Back Angle Exceed
        static WarGrey::SCADA::Alarm* SBDTCableExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarm>::UnsafeTongue(3927U); } // SB Drag Trunnion Cable Exceed

    private:
        Alarm(unsigned int idx) : Tongue(idx) {}
    };
}

