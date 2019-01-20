#pragma once

#include "tongue.hpp"

namespace WarGrey::SCADA {
    private class alarm : public WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm> {
        friend class WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>;
    public:
        static Platform::String^ type() { return "alarm"; }
        static unsigned int min_index() { return 39U; }
        static unsigned int max_index() { return 3927U; }

    public:
        static WarGrey::SCADA::alarm* PSHopperMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(39U); } // PS Hopper/Underwater Converter Mainenance
        static WarGrey::SCADA::alarm* PSGlandPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(43U); } // PS Master Gland Pump Broken
        static WarGrey::SCADA::alarm* PSGlandPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(47U); } // PS Spare Gland Pump Broken
        static WarGrey::SCADA::alarm* PSWaterPumpAlert() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(51U); } // PS Water Pump Alert
        static WarGrey::SCADA::alarm* PSWaterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(52U); } // PS Water Pump Broken
        static WarGrey::SCADA::alarm* PSWaterPumpMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(54U); } // PS Water Pump Maintenance
        static WarGrey::SCADA::alarm* PSWaterPumpEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(55U); } // PS Water Pump Emergence
        static WarGrey::SCADA::alarm* SBHopperMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(63U); } // SB Hopper/Underwater Converter Mainenance
        static WarGrey::SCADA::alarm* SBGlandPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(67U); } // SB Master Gland Pump Broken
        static WarGrey::SCADA::alarm* SBGlandPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(71U); } // SB Spare Gland Pump Broken
        static WarGrey::SCADA::alarm* SBWaterPumpAlert() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(75U); } // SB Water Pump Alert
        static WarGrey::SCADA::alarm* SBWaterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(76U); } // SB Water Pump Broken
        static WarGrey::SCADA::alarm* SBWaterPumpMaintenance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(79U); } // SB Water Pump Emergence
        static WarGrey::SCADA::alarm* PSDHPumpABroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(82U); } // PS Drag Head Pump A Broken
        static WarGrey::SCADA::alarm* PSDIPumpBBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(86U); } // PS Drag Intermediate Pump B Broken
        static WarGrey::SCADA::alarm* PSDTPumpCBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(90U); } // PS Drag Trunnion Pump C Broken
        static WarGrey::SCADA::alarm* SBDVPumpIBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(94U); } // SB Drag Visor Pump I Broken
        static WarGrey::SCADA::alarm* SBDHPumpHBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(98U); } // SB Drag Head Pump H Broken
        static WarGrey::SCADA::alarm* SBDIPumpGBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(102U); } // SB Drag Intermediate Pump G Broken
        static WarGrey::SCADA::alarm* SBDTPumpFBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(106U); } // SB Drag Trunnion Pump F Broken
        static WarGrey::SCADA::alarm* SBDVPumpJBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(110U); } // SB Drag Visor Pump J Broken
        static WarGrey::SCADA::alarm* DLBVPumpDBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(114U); } // Door Locker/Butterfly Valve Pump D Broken
        static WarGrey::SCADA::alarm* DLBVPumpDBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(118U); } // Door Locker/Butterfly Valve Pump E Broken
        static WarGrey::SCADA::alarm* CoolanPumpKBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(122U); } // Coolant Pump K Broken
        static WarGrey::SCADA::alarm* MotorFlushingPumpLBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(126U); } // Motor Flushing Pump L Broken
        static WarGrey::SCADA::alarm* SparePumpMBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(130U); } // Spare Pump M Broken
        static WarGrey::SCADA::alarm* EmergencePumpYBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(134U); } // Emergence Pump Y Broken
        static WarGrey::SCADA::alarm* PSGVFlushingPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(138U); } // PS Gate Valve Flushing Pump Broken
        static WarGrey::SCADA::alarm* SBGVFlushingPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(142U); } // SB Gate Valve Flushing Pump Broken
        static WarGrey::SCADA::alarm* MasterTankLS2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(150U); } // Master Tank Level Ultra Low
        static WarGrey::SCADA::alarm* VisorTankLS2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(162U); } // Drag Visor Tank Level Ultra Low
        static WarGrey::SCADA::alarm* PumpA2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(176U); } // Pump A Replace C
        static WarGrey::SCADA::alarm* PumpC2A() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(177U); } // Pump C Replace A
        static WarGrey::SCADA::alarm* PumpB2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(178U); } // Pump B Replace C
        static WarGrey::SCADA::alarm* PumpC2B() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(179U); } // Pump C Replace B
        static WarGrey::SCADA::alarm* PumpF2C() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(180U); } // Pump F Replace C
        static WarGrey::SCADA::alarm* PumpC2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(181U); } // Pump C Replace F
        static WarGrey::SCADA::alarm* PumpH2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(182U); } // Pump H Replace F
        static WarGrey::SCADA::alarm* PumpF2H() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(183U); } // Pump F Replace H
        static WarGrey::SCADA::alarm* PumpG2F() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(184U); } // Pump G Replace F
        static WarGrey::SCADA::alarm* PumpF2G() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(185U); } // Pump F Replace G
        static WarGrey::SCADA::alarm* PumpI2J() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(186U); } // Pump I Replace J
        static WarGrey::SCADA::alarm* PumpJ2I() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(187U); } // Pump J Replace I
        static WarGrey::SCADA::alarm* ShoreWinchEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(195U); } // Shore Dischange Winch Emergence
        static WarGrey::SCADA::alarm* PSDTEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(207U); } // PS Drag Trunnion Emergence
        static WarGrey::SCADA::alarm* PSDIEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(215U); } // PS Drag Intermediate Emergence
        static WarGrey::SCADA::alarm* PSDHEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(223U); } // PS Drag Head Emergence
        static WarGrey::SCADA::alarm* SBDTEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(231U); } // SB Drag Trunnion Emergence
        static WarGrey::SCADA::alarm* SBDTEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(239U); } // SB Drag Intermediate Emergence
        static WarGrey::SCADA::alarm* SBDHEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(247U); } // SB Drag Head Emergence
        static WarGrey::SCADA::alarm* BargeWinchEmergence() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(255U); } // Barge Winch Emergence
        static WarGrey::SCADA::alarm* PSHopperUnitPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(517U); } // PS Hopper Lubricating Unit Pressure Low
        static WarGrey::SCADA::alarm* PSHopperUnitLevelLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(518U); } // PS Hopper Lubricating Unit Level Low
        static WarGrey::SCADA::alarm* PSHopperUnitOilTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(519U); } // PS Hopper Lubricating Unit Oil Temperature High
        static WarGrey::SCADA::alarm* PSHopperUnitWaterTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(520U); } // PS Hopper Lubricating Unit Water Temperature High
        static WarGrey::SCADA::alarm* PSHopperUnitBearingTemperatureHigh1() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(521U); } // PS Hopper Lubricating Unit Bearing Temperature 1# High
        static WarGrey::SCADA::alarm* PSHopperUnitBearingTemperatureHigh2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(522U); } // PS Hopper Lubricating Unit Bearing Temperature 2# High
        static WarGrey::SCADA::alarm* PSHopperUnitBearingTemperatureHigh3() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(523U); } // PS Hopper Lubricating Unit Bearing Temperature 3# High
        static WarGrey::SCADA::alarm* SBHopperUnitPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(533U); } // SB Hopper Lubricating Unit Pressure Low
        static WarGrey::SCADA::alarm* SBHopperUnitLevelLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(534U); } // SB Hopper Lubricating Unit Level Low
        static WarGrey::SCADA::alarm* SBHopperUnitOilTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(535U); } // SB Hopper Lubricating Unit Oil Temperature High
        static WarGrey::SCADA::alarm* SBHopperUnitWaterTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(536U); } // SB Hopper Lubricating Unit Water Temperature High
        static WarGrey::SCADA::alarm* PSHopperUnitBearingTemperatureHigh1() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(537U); } // SB Hopper Lubricating Unit Bearing Temperature 1# High
        static WarGrey::SCADA::alarm* PSHopperUnitBearingTemperatureHigh2() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(538U); } // SB Hopper Lubricating Unit Bearing Temperature 2# High
        static WarGrey::SCADA::alarm* PSHopperUnitBearingTemperatureHigh3() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(539U); } // SB Hopper Lubricating Unit Bearing Temperature 3# High
        static WarGrey::SCADA::alarm* PSUnderWaterGlandPump1Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(547U); } // PS Underwater Master Gland Pump 1# Broken
        static WarGrey::SCADA::alarm* PSUnderWaterGlandPump2Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(551U); } // PS Underwater Master Gland Pump 2# Broken
        static WarGrey::SCADA::alarm* PSHopperGearboxMasterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(554U); } // PS Hopper Gearbox Master Pump Broken
        static WarGrey::SCADA::alarm* PSHopperGearboxSparePumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(557U); } // PS Hopper Gearbox Spare Pump Broken
        static WarGrey::SCADA::alarm* PSHopperGearboxTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(558U); } // PS Hopper Gearbox Temperature High
        static WarGrey::SCADA::alarm* PSHopperGearboxPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(559U); } // PS Hopper Gearbox Pressure Low
        static WarGrey::SCADA::alarm* SBUnderWaterGlandPump1Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(563U); } // SB Underwater Master Gland Pump 1# Broken
        static WarGrey::SCADA::alarm* SBUnderWaterGlandPump1Broken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(567U); } // SB Underwater Master Gland Pump 2# Broken
        static WarGrey::SCADA::alarm* SBHopperGearboxMasterPumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(570U); } // SB Hopper Gearbox Master Pump Broken
        static WarGrey::SCADA::alarm* SBHopperGearboxSparePumpBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(573U); } // SB Hopper Gearbox Spare Pump Broken
        static WarGrey::SCADA::alarm* SBHopperGearboxTemperatureHigh() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(574U); } // SB Hopper Gearbox Temperature High
        static WarGrey::SCADA::alarm* SBHopperGearboxPressureLow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(575U); } // SB Hopper Gearbox Pressure Low
        static WarGrey::SCADA::alarm* HydraulicsStoppedBySailing() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(700U); } // Hydraulics Stopped(Sailing Console)
        static WarGrey::SCADA::alarm* PSDIAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3050U); } // PS Drag Intermediate Angle Exceed
        static WarGrey::SCADA::alarm* SBDIAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3051U); } // SB Drag Intermediate Angle Exceed
        static WarGrey::SCADA::alarm* BackOilPressureFail() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3441U); } // Back Oil Pressure Less Than 3bar, All Winches are Limited
        static WarGrey::SCADA::alarm* ET200M_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3688U); } // 11#ET200M - Bow PLC 1 Lost
        static WarGrey::SCADA::alarm* ET200M_BowPLC2Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3689U); } // 12#ET200M - Bow PLC 2 ET200M Lost
        static WarGrey::SCADA::alarm* ET200M_BowPLC3Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3690U); } // 13#ET200M - Bow PLC 3 ET200M Lost
        static WarGrey::SCADA::alarm* ET200M_BowPLC4Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3691U); } // 14#ET200M - Bow PLC 4 ET200M Lost
        static WarGrey::SCADA::alarm* ET200M_BowPLC5Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3692U); } // 15#ET200M - Bow PLC 5 ET200M Lost
        static WarGrey::SCADA::alarm* ET200M_ConsoleDCC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3693U); } // 16#ET200M - Dredging Console DCC1 Lost
        static WarGrey::SCADA::alarm* ET200M_ConsoleDCC2Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3694U); } // 17#ET200M - Dredging Console DCC2 Lost
        static WarGrey::SCADA::alarm* PSDTWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3695U); } // 21# - PS Drag Trunnion Winch Lost
        static WarGrey::SCADA::alarm* SBDTWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3696U); } // 22# - SB Drag Trunnion Winch Lost
        static WarGrey::SCADA::alarm* PSDIWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3697U); } // 23# - PS Drag Intermediate Winch Lost
        static WarGrey::SCADA::alarm* SBDIWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3698U); } // 24# - SB Drag Intermediate Winch Lost
        static WarGrey::SCADA::alarm* PSDHWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3699U); } // 25# - PS Drag Head Winch Lost
        static WarGrey::SCADA::alarm* SBDHWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3700U); } // 26# - SB Drag Head Winch Lost
        static WarGrey::SCADA::alarm* BargeWinchLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3701U); } // 27# - Barge Winch Lost
        static WarGrey::SCADA::alarm* MasterCPU_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3702U); } // 2#CPU(Master) - Bow PLC 1 Lost
        static WarGrey::SCADA::alarm* SpareCPU_BowPLC1Lost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3703U); } // 3#CPU(Spare) - Bow PLC 1 Lost
        static WarGrey::SCADA::alarm* DCSDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3704U); } // 18# DCS DP Lost
        static WarGrey::SCADA::alarm* PSHPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3705U); } // 31# PS Hopper Pump DP Lost
        static WarGrey::SCADA::alarm* SBHPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3706U); } // 32# SB Hopper Pump DP Lost
        static WarGrey::SCADA::alarm* PSWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3707U); } // 33# PS Water Pump DP Lost
        static WarGrey::SCADA::alarm* SBWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3708U); } // 34# SB Water Pump DP Lost
        static WarGrey::SCADA::alarm* PSUWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3709U); } // 35# PS Underwater Pump DP Lost
        static WarGrey::SCADA::alarm* SBUWPDPLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3710U); } // 36# PS Underwater Pump DP Lost
        static WarGrey::SCADA::alarm* PLCPowerBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3711U); } // PLC Power Broken
        static WarGrey::SCADA::alarm* TildemeterLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3713U); } // Tildemeter Lost
        static WarGrey::SCADA::alarm* GPSLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3714U); } // GPS Lost
        static WarGrey::SCADA::alarm* GyrocompassLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3715U); } // Gyrocompass Lost
        static WarGrey::SCADA::alarm* PSFAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3920U); } // PS Forearm Drag Angle Exceed
        static WarGrey::SCADA::alarm* PSBAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3921U); } // PS Backarm Drag Angle Exceed
        static WarGrey::SCADA::alarm* PSFBAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3922U); } // PS Fore-Back Angle Exceed
        static WarGrey::SCADA::alarm* PSDTCableExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3923U); } // PS Drag Trunnion Cable Exceed
        static WarGrey::SCADA::alarm* SBFAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3924U); } // SB Forearm Drag Angle Exceed
        static WarGrey::SCADA::alarm* SBBAAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3925U); } // SB Backarm Drag Angle Exceed
        static WarGrey::SCADA::alarm* SBFBAngleExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3926U); } // SB Fore-Back Angle Exceed
        static WarGrey::SCADA::alarm* SBDTCableExceed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::alarm>::UnsafeTongue(3927U); } // SB Drag Trunnion Cable Exceed

    private:
        alarm(unsigned int idx) : Tongue(idx) {}
    };
}

