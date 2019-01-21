#pragma once

#include "tongue.hpp"

namespace WarGrey::SCADA {
    private class Alarms : public WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms> {
        friend class WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>;
    public:
        static Platform::String^ type() { return "alarm"; }

    public:
        static WarGrey::SCADA::Alarms* PSHopperMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(7U); } // PS Hopper/Underwater Converter Mainenance
        static WarGrey::SCADA::Alarms* PSGlandPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(11U); } // PS Master Gland Pump Broken
        static WarGrey::SCADA::Alarms* PSGlandPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(15U); } // PS Spare Gland Pump Broken
        static WarGrey::SCADA::Alarms* PSWaterPumpAlert() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(19U); } // PS Water Pump Alert
        static WarGrey::SCADA::Alarms* PSWaterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(20U); } // PS Water Pump Broken
        static WarGrey::SCADA::Alarms* PSWaterPumpMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(22U); } // PS Water Pump Maintenance
        static WarGrey::SCADA::Alarms* PSWaterPumpEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(23U); } // PS Water Pump Emergence
        static WarGrey::SCADA::Alarms* SBHopperMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(31U); } // SB Hopper/Underwater Converter Mainenance
        static WarGrey::SCADA::Alarms* SBGlandPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(35U); } // SB Master Gland Pump Broken
        static WarGrey::SCADA::Alarms* SBGlandPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(39U); } // SB Spare Gland Pump Broken
        static WarGrey::SCADA::Alarms* SBWaterPumpAlert() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(43U); } // SB Water Pump Alert
        static WarGrey::SCADA::Alarms* SBWaterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(44U); } // SB Water Pump Broken
        static WarGrey::SCADA::Alarms* SBWaterPumpMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(47U); } // SB Water Pump Emergence
        static WarGrey::SCADA::Alarms* PSDHPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(50U); } // PS Drag Head Pump A Broken
        static WarGrey::SCADA::Alarms* PSDIPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(54U); } // PS Drag Intermediate Pump B Broken
        static WarGrey::SCADA::Alarms* PSDTPumpCBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(58U); } // PS Drag Trunnion Pump C Broken
        static WarGrey::SCADA::Alarms* SBDVPumpIBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(62U); } // SB Drag Visor Pump I Broken
        static WarGrey::SCADA::Alarms* SBDHPumpHBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(66U); } // SB Drag Head Pump H Broken
        static WarGrey::SCADA::Alarms* SBDIPumpGBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(70U); } // SB Drag Intermediate Pump G Broken
        static WarGrey::SCADA::Alarms* SBDTPumpFBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(74U); } // SB Drag Trunnion Pump F Broken
        static WarGrey::SCADA::Alarms* SBDVPumpJBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(78U); } // SB Drag Visor Pump J Broken
        static WarGrey::SCADA::Alarms* DLBVPumpDBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(82U); } // Door Locker/Butterfly Valve Pump D Broken
        static WarGrey::SCADA::Alarms* DLBVPumpEBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(86U); } // Door Locker/Butterfly Valve Pump E Broken
        static WarGrey::SCADA::Alarms* CoolanPumpKBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(90U); } // Coolant Pump K Broken
        static WarGrey::SCADA::Alarms* MotorFlushingPumpLBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(94U); } // Motor Flushing Pump L Broken
        static WarGrey::SCADA::Alarms* SparePumpMBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(98U); } // Spare Pump M Broken
        static WarGrey::SCADA::Alarms* EmergencePumpYBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(102U); } // Emergence Pump Y Broken
        static WarGrey::SCADA::Alarms* PSGVFlushingPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(106U); } // PS Gate Valve Flushing Pump Broken
        static WarGrey::SCADA::Alarms* SBGVFlushingPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(110U); } // SB Gate Valve Flushing Pump Broken
        static WarGrey::SCADA::Alarms* MasterTankLS2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(118U); } // Master Tank Level Ultra Low
        static WarGrey::SCADA::Alarms* VisorTankLS2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(130U); } // Drag Visor Tank Level Ultra Low
        static WarGrey::SCADA::Alarms* PumpA2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(144U); } // Pump A Replace C
        static WarGrey::SCADA::Alarms* PumpC2A() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(145U); } // Pump C Replace A
        static WarGrey::SCADA::Alarms* PumpB2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(146U); } // Pump B Replace C
        static WarGrey::SCADA::Alarms* PumpC2B() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(147U); } // Pump C Replace B
        static WarGrey::SCADA::Alarms* PumpF2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(148U); } // Pump F Replace C
        static WarGrey::SCADA::Alarms* PumpC2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(149U); } // Pump C Replace F
        static WarGrey::SCADA::Alarms* PumpH2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(150U); } // Pump H Replace F
        static WarGrey::SCADA::Alarms* PumpF2H() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(151U); } // Pump F Replace H
        static WarGrey::SCADA::Alarms* PumpG2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(152U); } // Pump G Replace F
        static WarGrey::SCADA::Alarms* PumpF2G() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(153U); } // Pump F Replace G
        static WarGrey::SCADA::Alarms* PumpI2J() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(154U); } // Pump I Replace J
        static WarGrey::SCADA::Alarms* PumpJ2I() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(155U); } // Pump J Replace I
        static WarGrey::SCADA::Alarms* ShoreWinchEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(163U); } // Shore Dischange Winch Emergence
        static WarGrey::SCADA::Alarms* PSDTEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(175U); } // PS Drag Trunnion Emergence
        static WarGrey::SCADA::Alarms* PSDIEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(183U); } // PS Drag Intermediate Emergence
        static WarGrey::SCADA::Alarms* PSDHEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(191U); } // PS Drag Head Emergence
        static WarGrey::SCADA::Alarms* SBDTEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(199U); } // SB Drag Trunnion Emergence
        static WarGrey::SCADA::Alarms* SBDIEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(207U); } // SB Drag Intermediate Emergence
        static WarGrey::SCADA::Alarms* SBDHEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(215U); } // SB Drag Head Emergence
        static WarGrey::SCADA::Alarms* BargeWinchEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(223U); } // Barge Winch Emergence
        static WarGrey::SCADA::Alarms* PSHopperUnitPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(485U); } // PS Hopper Lubricating Unit Pressure Low
        static WarGrey::SCADA::Alarms* PSHopperUnitLevelLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(486U); } // PS Hopper Lubricating Unit Level Low
        static WarGrey::SCADA::Alarms* PSHopperUnitOilTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(487U); } // PS Hopper Lubricating Unit Oil Temperature High
        static WarGrey::SCADA::Alarms* PSHopperUnitWaterTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(488U); } // PS Hopper Lubricating Unit Water Temperature High
        static WarGrey::SCADA::Alarms* PSHopperUnitBearingTemperatureHigh1() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(489U); } // PS Hopper Lubricating Unit Bearing Temperature 1# High
        static WarGrey::SCADA::Alarms* PSHopperUnitBearingTemperatureHigh2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(490U); } // PS Hopper Lubricating Unit Bearing Temperature 2# High
        static WarGrey::SCADA::Alarms* PSHopperUnitBearingTemperatureHigh3() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(491U); } // PS Hopper Lubricating Unit Bearing Temperature 3# High
        static WarGrey::SCADA::Alarms* SBHopperUnitPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(501U); } // SB Hopper Lubricating Unit Pressure Low
        static WarGrey::SCADA::Alarms* SBHopperUnitLevelLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(502U); } // SB Hopper Lubricating Unit Level Low
        static WarGrey::SCADA::Alarms* SBHopperUnitOilTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(503U); } // SB Hopper Lubricating Unit Oil Temperature High
        static WarGrey::SCADA::Alarms* SBHopperUnitWaterTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(504U); } // SB Hopper Lubricating Unit Water Temperature High
        static WarGrey::SCADA::Alarms* SBHopperUnitBearingTemperatureHigh1() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(505U); } // SB Hopper Lubricating Unit Bearing Temperature 1# High
        static WarGrey::SCADA::Alarms* SBHopperUnitBearingTemperatureHigh2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(506U); } // SB Hopper Lubricating Unit Bearing Temperature 2# High
        static WarGrey::SCADA::Alarms* SBHopperUnitBearingTemperatureHigh3() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(507U); } // SB Hopper Lubricating Unit Bearing Temperature 3# High
        static WarGrey::SCADA::Alarms* PSUnderWaterGlandPump1Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(515U); } // PS Underwater Master Gland Pump 1# Broken
        static WarGrey::SCADA::Alarms* PSUnderWaterGlandPump2Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(519U); } // PS Underwater Master Gland Pump 2# Broken
        static WarGrey::SCADA::Alarms* PSHopperGearboxMasterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(522U); } // PS Hopper Gearbox Master Pump Broken
        static WarGrey::SCADA::Alarms* PSHopperGearboxSparePumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(525U); } // PS Hopper Gearbox Spare Pump Broken
        static WarGrey::SCADA::Alarms* PSHopperGearboxTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(526U); } // PS Hopper Gearbox Temperature High
        static WarGrey::SCADA::Alarms* PSHopperGearboxPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(527U); } // PS Hopper Gearbox Pressure Low
        static WarGrey::SCADA::Alarms* SBUnderWaterGlandPump1Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(531U); } // SB Underwater Master Gland Pump 1# Broken
        static WarGrey::SCADA::Alarms* SBUnderWaterGlandPump2Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(535U); } // SB Underwater Master Gland Pump 2# Broken
        static WarGrey::SCADA::Alarms* SBHopperGearboxMasterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(538U); } // SB Hopper Gearbox Master Pump Broken
        static WarGrey::SCADA::Alarms* SBHopperGearboxSparePumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(541U); } // SB Hopper Gearbox Spare Pump Broken
        static WarGrey::SCADA::Alarms* SBHopperGearboxTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(542U); } // SB Hopper Gearbox Temperature High
        static WarGrey::SCADA::Alarms* SBHopperGearboxPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(543U); } // SB Hopper Gearbox Pressure Low
        static WarGrey::SCADA::Alarms* HydraulicsStoppedBySailing() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(668U); } // Hydraulics Stopped(Sailing Console)
        static WarGrey::SCADA::Alarms* PSDIAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(1418U); } // PS Drag Intermediate Angle Exceed
        static WarGrey::SCADA::Alarms* SBDIAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(1419U); } // SB Drag Intermediate Angle Exceed
        static WarGrey::SCADA::Alarms* BackOilPressureFail() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(1809U); } // Back Oil Pressure Less Than 3bar, All Winches are Limited
        static WarGrey::SCADA::Alarms* ET200M_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2056U); } // 11#ET200M - Bow PLC 1 Lost
        static WarGrey::SCADA::Alarms* ET200M_BowPLC2Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2057U); } // 12#ET200M - Bow PLC 2 ET200M Lost
        static WarGrey::SCADA::Alarms* ET200M_BowPLC3Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2058U); } // 13#ET200M - Bow PLC 3 ET200M Lost
        static WarGrey::SCADA::Alarms* ET200M_BowPLC4Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2059U); } // 14#ET200M - Bow PLC 4 ET200M Lost
        static WarGrey::SCADA::Alarms* ET200M_BowPLC5Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2060U); } // 15#ET200M - Bow PLC 5 ET200M Lost
        static WarGrey::SCADA::Alarms* ET200M_ConsoleDCC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2061U); } // 16#ET200M - Dredging Console DCC1 Lost
        static WarGrey::SCADA::Alarms* ET200M_ConsoleDCC2Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2062U); } // 17#ET200M - Dredging Console DCC2 Lost
        static WarGrey::SCADA::Alarms* PSDTWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2063U); } // 21# - PS Drag Trunnion Winch Lost
        static WarGrey::SCADA::Alarms* SBDTWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2064U); } // 22# - SB Drag Trunnion Winch Lost
        static WarGrey::SCADA::Alarms* PSDIWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2065U); } // 23# - PS Drag Intermediate Winch Lost
        static WarGrey::SCADA::Alarms* SBDIWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2066U); } // 24# - SB Drag Intermediate Winch Lost
        static WarGrey::SCADA::Alarms* PSDHWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2067U); } // 25# - PS Drag Head Winch Lost
        static WarGrey::SCADA::Alarms* SBDHWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2068U); } // 26# - SB Drag Head Winch Lost
        static WarGrey::SCADA::Alarms* BargeWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2069U); } // 27# - Barge Winch Lost
        static WarGrey::SCADA::Alarms* MasterCPU_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2070U); } // 2#CPU(Master) - Bow PLC 1 Lost
        static WarGrey::SCADA::Alarms* SpareCPU_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2071U); } // 3#CPU(Spare) - Bow PLC 1 Lost
        static WarGrey::SCADA::Alarms* DCSDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2072U); } // 18# DCS DP Lost
        static WarGrey::SCADA::Alarms* PSHPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2073U); } // 31# PS Hopper Pump DP Lost
        static WarGrey::SCADA::Alarms* SBHPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2074U); } // 32# SB Hopper Pump DP Lost
        static WarGrey::SCADA::Alarms* PSWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2075U); } // 33# PS Water Pump DP Lost
        static WarGrey::SCADA::Alarms* SBWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2076U); } // 34# SB Water Pump DP Lost
        static WarGrey::SCADA::Alarms* PSUWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2077U); } // 35# PS Underwater Pump DP Lost
        static WarGrey::SCADA::Alarms* SBUWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2078U); } // 36# PS Underwater Pump DP Lost
        static WarGrey::SCADA::Alarms* PLCPowerBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2079U); } // PLC Power Broken
        static WarGrey::SCADA::Alarms* TildemeterLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2081U); } // Tildemeter Lost
        static WarGrey::SCADA::Alarms* GPSLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2082U); } // GPS Lost
        static WarGrey::SCADA::Alarms* GyrocompassLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2083U); } // Gyrocompass Lost
        static WarGrey::SCADA::Alarms* PSFAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2288U); } // PS Forearm Drag Angle Exceed
        static WarGrey::SCADA::Alarms* PSBAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2289U); } // PS Backarm Drag Angle Exceed
        static WarGrey::SCADA::Alarms* PSFBAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2290U); } // PS Fore-Back Angle Exceed
        static WarGrey::SCADA::Alarms* PSDTCableExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2291U); } // PS Drag Trunnion Cable Exceed
        static WarGrey::SCADA::Alarms* SBFAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2292U); } // SB Forearm Drag Angle Exceed
        static WarGrey::SCADA::Alarms* SBBAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2293U); } // SB Backarm Drag Angle Exceed
        static WarGrey::SCADA::Alarms* SBFBAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2294U); } // SB Fore-Back Angle Exceed
        static WarGrey::SCADA::Alarms* SBDTCableExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Alarms>::UnsafeTongue(2295U); } // SB Drag Trunnion Cable Exceed

    private:
        static size_t indices_size() { return 116U; }
        static const unsigned int* indices() {
            static const unsigned int indexes[] = {
                7U, 11U, 15U, 19U, 20U, 22U, 23U, 31U, 35U, 39U, 43U, 44U, 47U, 50U, 54U, 58U,
                62U, 66U, 70U, 74U, 78U, 82U, 86U, 90U, 94U, 98U, 102U, 106U, 110U, 118U, 130U, 144U,
                145U, 146U, 147U, 148U, 149U, 150U, 151U, 152U, 153U, 154U, 155U, 163U, 175U, 183U, 191U, 199U,
                207U, 215U, 223U, 485U, 486U, 487U, 488U, 489U, 490U, 491U, 501U, 502U, 503U, 504U, 505U, 506U,
                507U, 515U, 519U, 522U, 525U, 526U, 527U, 531U, 535U, 538U, 541U, 542U, 543U, 668U, 1418U, 1419U,
                1809U, 2056U, 2057U, 2058U, 2059U, 2060U, 2061U, 2062U, 2063U, 2064U, 2065U, 2066U, 2067U, 2068U, 2069U, 2070U,
                2071U, 2072U, 2073U, 2074U, 2075U, 2076U, 2077U, 2078U, 2079U, 2081U, 2082U, 2083U, 2288U, 2289U, 2290U, 2291U,
                2292U, 2293U, 2294U, 2295U
            };

            return indexes;
        }

    private:
        Alarms(unsigned int idx) : Tongue(idx) {}
    };
}

