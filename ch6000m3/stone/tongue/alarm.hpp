#pragma once

#include "tongue.hpp"

namespace WarGrey::SCADA {
    private class Alarms : public WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms> {
        friend class WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>;
    public:
        static Platform::String^ type() { return "alarm"; }

    public:
        static WarGrey::SCADA::Alarms* PSHopperMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(39U); } // PS Hopper/Underwater Converter Mainenance
        static WarGrey::SCADA::Alarms* PSGlandPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(43U); } // PS Master Gland Pump Broken
        static WarGrey::SCADA::Alarms* PSGlandPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(47U); } // PS Spare Gland Pump Broken
        static WarGrey::SCADA::Alarms* PSWaterPumpAlert() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(51U); } // PS Water Pump Alert
        static WarGrey::SCADA::Alarms* PSWaterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(52U); } // PS Water Pump Broken
        static WarGrey::SCADA::Alarms* PSWaterPumpMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(54U); } // PS Water Pump Maintenance
        static WarGrey::SCADA::Alarms* PSWaterPumpEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(55U); } // PS Water Pump Emergence
        static WarGrey::SCADA::Alarms* SBHopperMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(63U); } // SB Hopper/Underwater Converter Mainenance
        static WarGrey::SCADA::Alarms* SBGlandPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(67U); } // SB Master Gland Pump Broken
        static WarGrey::SCADA::Alarms* SBGlandPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(71U); } // SB Spare Gland Pump Broken
        static WarGrey::SCADA::Alarms* SBWaterPumpAlert() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(75U); } // SB Water Pump Alert
        static WarGrey::SCADA::Alarms* SBWaterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(76U); } // SB Water Pump Broken
        static WarGrey::SCADA::Alarms* SBWaterPumpMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(79U); } // SB Water Pump Emergence
        static WarGrey::SCADA::Alarms* PSDHPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(82U); } // PS Drag Head Pump A Broken
        static WarGrey::SCADA::Alarms* PSDIPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(86U); } // PS Drag Intermediate Pump B Broken
        static WarGrey::SCADA::Alarms* PSDTPumpCBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(90U); } // PS Drag Trunnion Pump C Broken
        static WarGrey::SCADA::Alarms* SBDVPumpIBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(94U); } // SB Drag Visor Pump I Broken
        static WarGrey::SCADA::Alarms* SBDHPumpHBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(98U); } // SB Drag Head Pump H Broken
        static WarGrey::SCADA::Alarms* SBDIPumpGBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(102U); } // SB Drag Intermediate Pump G Broken
        static WarGrey::SCADA::Alarms* SBDTPumpFBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(106U); } // SB Drag Trunnion Pump F Broken
        static WarGrey::SCADA::Alarms* SBDVPumpJBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(110U); } // SB Drag Visor Pump J Broken
        static WarGrey::SCADA::Alarms* DLBVPumpDBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(114U); } // Door Locker/Butterfly Valve Pump D Broken
        static WarGrey::SCADA::Alarms* DLBVPumpEBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(118U); } // Door Locker/Butterfly Valve Pump E Broken
        static WarGrey::SCADA::Alarms* CoolanPumpKBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(122U); } // Coolant Pump K Broken
        static WarGrey::SCADA::Alarms* MotorFlushingPumpLBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(126U); } // Motor Flushing Pump L Broken
        static WarGrey::SCADA::Alarms* SparePumpMBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(130U); } // Spare Pump M Broken
        static WarGrey::SCADA::Alarms* EmergencePumpYBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(134U); } // Emergence Pump Y Broken
        static WarGrey::SCADA::Alarms* PSGVFlushingPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(138U); } // PS Gate Valve Flushing Pump Broken
        static WarGrey::SCADA::Alarms* SBGVFlushingPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(142U); } // SB Gate Valve Flushing Pump Broken
        static WarGrey::SCADA::Alarms* MasterTankLS2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(150U); } // Master Tank Level Ultra Low
        static WarGrey::SCADA::Alarms* VisorTankLS2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(162U); } // Drag Visor Tank Level Ultra Low
        static WarGrey::SCADA::Alarms* PumpA2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(176U); } // Pump A Replace C
        static WarGrey::SCADA::Alarms* PumpC2A() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(177U); } // Pump C Replace A
        static WarGrey::SCADA::Alarms* PumpB2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(178U); } // Pump B Replace C
        static WarGrey::SCADA::Alarms* PumpC2B() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(179U); } // Pump C Replace B
        static WarGrey::SCADA::Alarms* PumpF2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(180U); } // Pump F Replace C
        static WarGrey::SCADA::Alarms* PumpC2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(181U); } // Pump C Replace F
        static WarGrey::SCADA::Alarms* PumpH2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(182U); } // Pump H Replace F
        static WarGrey::SCADA::Alarms* PumpF2H() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(183U); } // Pump F Replace H
        static WarGrey::SCADA::Alarms* PumpG2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(184U); } // Pump G Replace F
        static WarGrey::SCADA::Alarms* PumpF2G() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(185U); } // Pump F Replace G
        static WarGrey::SCADA::Alarms* PumpI2J() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(186U); } // Pump I Replace J
        static WarGrey::SCADA::Alarms* PumpJ2I() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(187U); } // Pump J Replace I
        static WarGrey::SCADA::Alarms* ShoreWinchEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(195U); } // Shore Dischange Winch Emergence
        static WarGrey::SCADA::Alarms* PSDTEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(207U); } // PS Drag Trunnion Emergence
        static WarGrey::SCADA::Alarms* PSDIEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(215U); } // PS Drag Intermediate Emergence
        static WarGrey::SCADA::Alarms* PSDHEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(223U); } // PS Drag Head Emergence
        static WarGrey::SCADA::Alarms* SBDTEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(231U); } // SB Drag Trunnion Emergence
        static WarGrey::SCADA::Alarms* SBDIEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(239U); } // SB Drag Intermediate Emergence
        static WarGrey::SCADA::Alarms* SBDHEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(247U); } // SB Drag Head Emergence
        static WarGrey::SCADA::Alarms* BargeWinchEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(255U); } // Barge Winch Emergence
        static WarGrey::SCADA::Alarms* PSHopperUnitPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(517U); } // PS Hopper Lubricating Unit Pressure Low
        static WarGrey::SCADA::Alarms* PSHopperUnitLevelLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(518U); } // PS Hopper Lubricating Unit Level Low
        static WarGrey::SCADA::Alarms* PSHopperUnitOilTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(519U); } // PS Hopper Lubricating Unit Oil Temperature High
        static WarGrey::SCADA::Alarms* PSHopperUnitWaterTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(520U); } // PS Hopper Lubricating Unit Water Temperature High
        static WarGrey::SCADA::Alarms* PSHopperUnitBearingTemperatureHigh1() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(521U); } // PS Hopper Lubricating Unit Bearing Temperature 1# High
        static WarGrey::SCADA::Alarms* PSHopperUnitBearingTemperatureHigh2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(522U); } // PS Hopper Lubricating Unit Bearing Temperature 2# High
        static WarGrey::SCADA::Alarms* PSHopperUnitBearingTemperatureHigh3() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(523U); } // PS Hopper Lubricating Unit Bearing Temperature 3# High
        static WarGrey::SCADA::Alarms* SBHopperUnitPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(533U); } // SB Hopper Lubricating Unit Pressure Low
        static WarGrey::SCADA::Alarms* SBHopperUnitLevelLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(534U); } // SB Hopper Lubricating Unit Level Low
        static WarGrey::SCADA::Alarms* SBHopperUnitOilTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(535U); } // SB Hopper Lubricating Unit Oil Temperature High
        static WarGrey::SCADA::Alarms* SBHopperUnitWaterTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(536U); } // SB Hopper Lubricating Unit Water Temperature High
        static WarGrey::SCADA::Alarms* SBHopperUnitBearingTemperatureHigh1() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(537U); } // SB Hopper Lubricating Unit Bearing Temperature 1# High
        static WarGrey::SCADA::Alarms* SBHopperUnitBearingTemperatureHigh2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(538U); } // SB Hopper Lubricating Unit Bearing Temperature 2# High
        static WarGrey::SCADA::Alarms* SBHopperUnitBearingTemperatureHigh3() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(539U); } // SB Hopper Lubricating Unit Bearing Temperature 3# High
        static WarGrey::SCADA::Alarms* PSUnderWaterGlandPump1Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(547U); } // PS Underwater Master Gland Pump 1# Broken
        static WarGrey::SCADA::Alarms* PSUnderWaterGlandPump2Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(551U); } // PS Underwater Master Gland Pump 2# Broken
        static WarGrey::SCADA::Alarms* PSHopperGearboxMasterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(554U); } // PS Hopper Gearbox Master Pump Broken
        static WarGrey::SCADA::Alarms* PSHopperGearboxSparePumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(557U); } // PS Hopper Gearbox Spare Pump Broken
        static WarGrey::SCADA::Alarms* PSHopperGearboxTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(558U); } // PS Hopper Gearbox Temperature High
        static WarGrey::SCADA::Alarms* PSHopperGearboxPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(559U); } // PS Hopper Gearbox Pressure Low
        static WarGrey::SCADA::Alarms* SBUnderWaterGlandPump1Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(563U); } // SB Underwater Master Gland Pump 1# Broken
        static WarGrey::SCADA::Alarms* SBUnderWaterGlandPump2Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(567U); } // SB Underwater Master Gland Pump 2# Broken
        static WarGrey::SCADA::Alarms* SBHopperGearboxMasterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(570U); } // SB Hopper Gearbox Master Pump Broken
        static WarGrey::SCADA::Alarms* SBHopperGearboxSparePumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(573U); } // SB Hopper Gearbox Spare Pump Broken
        static WarGrey::SCADA::Alarms* SBHopperGearboxTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(574U); } // SB Hopper Gearbox Temperature High
        static WarGrey::SCADA::Alarms* SBHopperGearboxPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(575U); } // SB Hopper Gearbox Pressure Low
        static WarGrey::SCADA::Alarms* HydraulicsStoppedBySailing() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(700U); } // Hydraulics Stopped(Sailing Console)
        static WarGrey::SCADA::Alarms* PSDIAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3050U); } // PS Drag Intermediate Angle Exceed
        static WarGrey::SCADA::Alarms* SBDIAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3051U); } // SB Drag Intermediate Angle Exceed
        static WarGrey::SCADA::Alarms* BackOilPressureFail() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3441U); } // Back Oil Pressure Less Than 3bar, All Winches are Limited
        static WarGrey::SCADA::Alarms* ET200M_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3688U); } // 11#ET200M - Bow PLC 1 Lost
        static WarGrey::SCADA::Alarms* ET200M_BowPLC2Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3689U); } // 12#ET200M - Bow PLC 2 ET200M Lost
        static WarGrey::SCADA::Alarms* ET200M_BowPLC3Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3690U); } // 13#ET200M - Bow PLC 3 ET200M Lost
        static WarGrey::SCADA::Alarms* ET200M_BowPLC4Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3691U); } // 14#ET200M - Bow PLC 4 ET200M Lost
        static WarGrey::SCADA::Alarms* ET200M_BowPLC5Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3692U); } // 15#ET200M - Bow PLC 5 ET200M Lost
        static WarGrey::SCADA::Alarms* ET200M_ConsoleDCC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3693U); } // 16#ET200M - Dredging Console DCC1 Lost
        static WarGrey::SCADA::Alarms* ET200M_ConsoleDCC2Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3694U); } // 17#ET200M - Dredging Console DCC2 Lost
        static WarGrey::SCADA::Alarms* PSDTWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3695U); } // 21# - PS Drag Trunnion Winch Lost
        static WarGrey::SCADA::Alarms* SBDTWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3696U); } // 22# - SB Drag Trunnion Winch Lost
        static WarGrey::SCADA::Alarms* PSDIWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3697U); } // 23# - PS Drag Intermediate Winch Lost
        static WarGrey::SCADA::Alarms* SBDIWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3698U); } // 24# - SB Drag Intermediate Winch Lost
        static WarGrey::SCADA::Alarms* PSDHWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3699U); } // 25# - PS Drag Head Winch Lost
        static WarGrey::SCADA::Alarms* SBDHWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3700U); } // 26# - SB Drag Head Winch Lost
        static WarGrey::SCADA::Alarms* BargeWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3701U); } // 27# - Barge Winch Lost
        static WarGrey::SCADA::Alarms* MasterCPU_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3702U); } // 2#CPU(Master) - Bow PLC 1 Lost
        static WarGrey::SCADA::Alarms* SpareCPU_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3703U); } // 3#CPU(Spare) - Bow PLC 1 Lost
        static WarGrey::SCADA::Alarms* DCSDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3704U); } // 18# DCS DP Lost
        static WarGrey::SCADA::Alarms* PSHPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3705U); } // 31# PS Hopper Pump DP Lost
        static WarGrey::SCADA::Alarms* SBHPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3706U); } // 32# SB Hopper Pump DP Lost
        static WarGrey::SCADA::Alarms* PSWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3707U); } // 33# PS Water Pump DP Lost
        static WarGrey::SCADA::Alarms* SBWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3708U); } // 34# SB Water Pump DP Lost
        static WarGrey::SCADA::Alarms* PSUWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3709U); } // 35# PS Underwater Pump DP Lost
        static WarGrey::SCADA::Alarms* SBUWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3710U); } // 36# PS Underwater Pump DP Lost
        static WarGrey::SCADA::Alarms* PLCPowerBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3711U); } // PLC Power Broken
        static WarGrey::SCADA::Alarms* TildemeterLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3713U); } // Tildemeter Lost
        static WarGrey::SCADA::Alarms* GPSLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3714U); } // GPS Lost
        static WarGrey::SCADA::Alarms* GyrocompassLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3715U); } // Gyrocompass Lost
        static WarGrey::SCADA::Alarms* PSFAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3920U); } // PS Forearm Drag Angle Exceed
        static WarGrey::SCADA::Alarms* PSBAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3921U); } // PS Backarm Drag Angle Exceed
        static WarGrey::SCADA::Alarms* PSFBAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3922U); } // PS Fore-Back Angle Exceed
        static WarGrey::SCADA::Alarms* PSDTCableExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3923U); } // PS Drag Trunnion Cable Exceed
        static WarGrey::SCADA::Alarms* SBFAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3924U); } // SB Forearm Drag Angle Exceed
        static WarGrey::SCADA::Alarms* SBBAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3925U); } // SB Backarm Drag Angle Exceed
        static WarGrey::SCADA::Alarms* SBFBAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3926U); } // SB Fore-Back Angle Exceed
        static WarGrey::SCADA::Alarms* SBDTCableExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(3927U); } // SB Drag Trunnion Cable Exceed

    private:
        static size_t indices_size() { return 116U; }
        static const unsigned int* indices() {
            static const unsigned int indexes[] = {
                39U, 43U, 47U, 51U, 52U, 54U, 55U, 63U, 67U, 71U, 75U, 76U, 79U, 82U, 86U, 90U,
                94U, 98U, 102U, 106U, 110U, 114U, 118U, 122U, 126U, 130U, 134U, 138U, 142U, 150U, 162U, 176U,
                177U, 178U, 179U, 180U, 181U, 182U, 183U, 184U, 185U, 186U, 187U, 195U, 207U, 215U, 223U, 231U,
                239U, 247U, 255U, 517U, 518U, 519U, 520U, 521U, 522U, 523U, 533U, 534U, 535U, 536U, 537U, 538U,
                539U, 547U, 551U, 554U, 557U, 558U, 559U, 563U, 567U, 570U, 573U, 574U, 575U, 700U, 3050U, 3051U,
                3441U, 3688U, 3689U, 3690U, 3691U, 3692U, 3693U, 3694U, 3695U, 3696U, 3697U, 3698U, 3699U, 3700U, 3701U, 3702U,
                3703U, 3704U, 3705U, 3706U, 3707U, 3708U, 3709U, 3710U, 3711U, 3713U, 3714U, 3715U, 3920U, 3921U, 3922U, 3923U,
                3924U, 3925U, 3926U, 3927U
            };

            return indexes;
        }

    private:
        Alarms(unsigned int idx) : Tongue(idx) {}
    };
}

