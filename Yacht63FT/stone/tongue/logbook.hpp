#pragma once

#include "tongue.hpp"

namespace WarGrey::SCADA {
    private class Logbook : public WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook> {
        friend class WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>;
    public:
        static Platform::String^ type() { return "logbook"; }

    public:
        static WarGrey::SCADA::Logbook* FWCoolant1Running() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(1U); } // Fresh Water Cooling Pump #1 Running
        static WarGrey::SCADA::Logbook* FWCoolant1Fault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(2U); } // Fresh Water Cooling Pump #1 Fault
        static WarGrey::SCADA::Logbook* FWCoolant2Running() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(3U); } // Fresh Water Cooling Pump #2 Running
        static WarGrey::SCADA::Logbook* FWCoolant2Fault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(4U); } // Fresh Water Cooling Pump #2 Fault
        static WarGrey::SCADA::Logbook* SWCoolant1Running() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(5U); } // Sea Water Cooling Pump #1 Running
        static WarGrey::SCADA::Logbook* SWCoolant1Fault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(6U); } // Sea Water Cooling Pump #1 Fault
        static WarGrey::SCADA::Logbook* SWCoolant2Running() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(7U); } // Sea Water Cooling Pump #2 Running
        static WarGrey::SCADA::Logbook* SWCoolant2Fault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(8U); } // Sea Water Cooling Pump #2 Fault
        static WarGrey::SCADA::Logbook* DC24VBPLowResistance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(9U); } // DC24V Breaker Panel Resistance Low
        static WarGrey::SCADA::Logbook* AC220VBPLowResistance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(10U); } // AC220V Breaker Panel Resistance Low
        static WarGrey::SCADA::Logbook* WaterWorksRunning() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(11U); } // WaterWorks Running
        static WarGrey::SCADA::Logbook* WaterWorksFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(12U); } // WaterWorks Fault
        static WarGrey::SCADA::Logbook* ACCoolantRunning() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(17U); } // Air Conditioner Cooling Pump Running
        static WarGrey::SCADA::Logbook* ACCoolantFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(18U); } // Air Conditioner Cooling Pump Fault
        static WarGrey::SCADA::Logbook* CO2PowerFailure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(19U); } // CO2 Power Failure
        static WarGrey::SCADA::Logbook* CO2Leak() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(20U); } // CO2 Leak
        static WarGrey::SCADA::Logbook* CO2Release() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(21U); } // CO2 Release
        static WarGrey::SCADA::Logbook* M1PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(25U); } // M1 Power Module Fault
        static WarGrey::SCADA::Logbook* PSBusOverVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(26U); } // Port Station DC Bus Voltage Exceed
        static WarGrey::SCADA::Logbook* PSBusUnderVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(27U); } // Port Station DC Bus Voltage Beneath
        static WarGrey::SCADA::Logbook* M1IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(28U); } // M1 Instantaneous Over Current Relay Fault
        static WarGrey::SCADA::Logbook* M1DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(29U); } // M1 Definite Time Over Current Relay Fault
        static WarGrey::SCADA::Logbook* M1MotorOverSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(30U); } // M1 Motor Rotational Speed Exceed
        static WarGrey::SCADA::Logbook* M1MotorUnderSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(31U); } // M1 Motor Rotational Speed Beneath
        static WarGrey::SCADA::Logbook* M1CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(32U); } // M1 CB1 Over Current Protective Relay Tripped Off
        static WarGrey::SCADA::Logbook* M1DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(33U); } // M1 DC Fuse Broken
        static WarGrey::SCADA::Logbook* M1UpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(34U); } // M1 U Phase Winding Temperature Exceed
        static WarGrey::SCADA::Logbook* M1VpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(35U); } // M1 V Phase Winding Temperature Exceed
        static WarGrey::SCADA::Logbook* M1WpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(36U); } // M1 W Phase Winding Temperature Exceed
        static WarGrey::SCADA::Logbook* M1DBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(37U); } // M1 Driven Bearing Temperature Exceed
        static WarGrey::SCADA::Logbook* M1nDBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(38U); } // M1 non-Driven Bearing Temperature Exceed
        static WarGrey::SCADA::Logbook* M1TBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(39U); } // M1 Thrust Bearing Temperature Exceed
        static WarGrey::SCADA::Logbook* M1PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(40U); } // M1 Power Module Temperature Exceed
        static WarGrey::SCADA::Logbook* PSBusHighVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(41U); } // Port Station DC Bus Voltage High
        static WarGrey::SCADA::Logbook* PSBusLowVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(42U); } // Port Station DC Bus Voltage Low
        static WarGrey::SCADA::Logbook* M1Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(43U); } // M1 Overload
        static WarGrey::SCADA::Logbook* M1VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(44U); } // M1 Current Very High
        static WarGrey::SCADA::Logbook* M1PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(45U); } // M1 Power Module Temperature High
        static WarGrey::SCADA::Logbook* M1PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(46U); } // M1 Power Module Temperature Very High
        static WarGrey::SCADA::Logbook* M1MotorLocked() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(47U); } // M1 Motor Locked
        static WarGrey::SCADA::Logbook* M1UpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(48U); } // M1 U Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* M1VpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(49U); } // M1 V Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* M1WpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(50U); } // M1 W Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* M1DBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(51U); } // M1 Driven Bearing Temperature High
        static WarGrey::SCADA::Logbook* M1nDBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(52U); } // M1 non-Driven Bearing Temperature High
        static WarGrey::SCADA::Logbook* M1TBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(53U); } // M1 Thrust Bearing Temperature High
        static WarGrey::SCADA::Logbook* M1UpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(54U); } // M1 U Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* M1VpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(55U); } // M1 V Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* M1WpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(56U); } // M1 W Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* M1DBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(57U); } // M1 Driven Bearing Temperature Very High
        static WarGrey::SCADA::Logbook* M1nDBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(58U); } // M1 non-Driven Bearing Temperature Very High
        static WarGrey::SCADA::Logbook* M1TBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(59U); } // M1 Thrust Bearing Temperature Very High
        static WarGrey::SCADA::Logbook* M1Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(60U); } // M1 Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* M2PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(61U); } // M2 Power Module Fault
        static WarGrey::SCADA::Logbook* SBBusOverVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(62U); } // Starboard DC Bus Voltage Exceed
        static WarGrey::SCADA::Logbook* SBBusUnderVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(63U); } // Starboard DC Bus Voltage Beneath
        static WarGrey::SCADA::Logbook* M2IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(64U); } // M2 Instantaneous Over Current Relay Fault
        static WarGrey::SCADA::Logbook* M2DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(65U); } // M2 Definite Time Over Current Relay Fault
        static WarGrey::SCADA::Logbook* M2MotorOverSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(66U); } // M2 Motor Rotational Speed Exceed
        static WarGrey::SCADA::Logbook* M2MotorUnderSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(67U); } // M2 Motor Rotational Speed Beneath
        static WarGrey::SCADA::Logbook* M2CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(68U); } // M2 CB1 Over Current Protective Relay Tripped Off
        static WarGrey::SCADA::Logbook* M2DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(69U); } // M2 DC Fuse Broken
        static WarGrey::SCADA::Logbook* M2UOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(70U); } // M2 U Temperature Exceed
        static WarGrey::SCADA::Logbook* M2VOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(71U); } // M2 V Temperature Exceed
        static WarGrey::SCADA::Logbook* M2WOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(72U); } // M2 W Temperature Exceed
        static WarGrey::SCADA::Logbook* M2DBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(73U); } // M2 Driven Bearing Temperature Exceed
        static WarGrey::SCADA::Logbook* M2nDBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(74U); } // M2 non-Driven Bearing Temperature Exceed
        static WarGrey::SCADA::Logbook* M2TBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(75U); } // M2 Thrust Bearing Temperature Exceed
        static WarGrey::SCADA::Logbook* M2PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(76U); } // M2 Power Module Temperature Exceed
        static WarGrey::SCADA::Logbook* SBBusHighVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(77U); } // Starboard DC Bus Voltage High
        static WarGrey::SCADA::Logbook* SBBusLowVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(78U); } // Starboard DC Bus Voltage Low
        static WarGrey::SCADA::Logbook* M2Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(79U); } // M2 Overload
        static WarGrey::SCADA::Logbook* M2VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(80U); } // M2 Current Very High
        static WarGrey::SCADA::Logbook* M2PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(81U); } // M2 Power Module Temperature High
        static WarGrey::SCADA::Logbook* M2PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(82U); } // M2 Power Module Temperature Very High
        static WarGrey::SCADA::Logbook* M2MotorLocked() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(83U); } // M2 Motor Locked
        static WarGrey::SCADA::Logbook* M2UpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(84U); } // M2 U Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* M2VpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(85U); } // M2 V Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* M2WpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(86U); } // M2 W Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* M2DBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(87U); } // M2 Driven Bearing Temperature High
        static WarGrey::SCADA::Logbook* M2nDBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(88U); } // M2 non-Driven Bearing Temperature High
        static WarGrey::SCADA::Logbook* M2TBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(89U); } // M2 Thrust Bearing Temperature High
        static WarGrey::SCADA::Logbook* M2UpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(90U); } // M2 U Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* M2VpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(91U); } // M2 V Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* M2WpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(92U); } // M2 W Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* M2DBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(93U); } // M2 Driven Bearing Temperature Very High
        static WarGrey::SCADA::Logbook* M2nDBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(94U); } // M2 non-Driven Bearing Temperature Very High
        static WarGrey::SCADA::Logbook* M2TBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(95U); } // M2 Thrust Bearing Temperature Very High
        static WarGrey::SCADA::Logbook* M2Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(96U); } // M2 Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* G1PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(105U); } // G1 Power Module Fault
        static WarGrey::SCADA::Logbook* G1IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(106U); } // G1 Instantaneous Over Current Relay Fault
        static WarGrey::SCADA::Logbook* G1DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(107U); } // G1 Definite Time Over Current Relay Fault
        static WarGrey::SCADA::Logbook* G1MotorOverSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(108U); } // G1 Motor Rotational Speed Exceed
        static WarGrey::SCADA::Logbook* G1MotorUnderSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(109U); } // G1 Motor Rotational Speed Beneath
        static WarGrey::SCADA::Logbook* G1Diesel1CMisc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(110U); } // G1 Diesel 1st Class Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* G1CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(111U); } // G1 CB1 Over Current Protective Relay Tripped Off
        static WarGrey::SCADA::Logbook* G1DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(112U); } // G1 DC Fuse Broken
        static WarGrey::SCADA::Logbook* G1UpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(113U); } // G1 U Phase Winding Temperature Exceed
        static WarGrey::SCADA::Logbook* G1VpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(114U); } // G1 V Phase Winding Temperature Exceed
        static WarGrey::SCADA::Logbook* G1WpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(115U); } // G1 W Phase Winding Temperature Exceed
        static WarGrey::SCADA::Logbook* G1DBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(116U); } // G1 Driven Bearing Temperature Exceed
        static WarGrey::SCADA::Logbook* G1nDBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(117U); } // G1 non-Driven Bearing Temperature Exceed
        static WarGrey::SCADA::Logbook* G1SPTransformerOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(118U); } // G1 Shore Power Transformer Temperature Exceed
        static WarGrey::SCADA::Logbook* G1PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(119U); } // G1 Power Module Temperature Exceed
        static WarGrey::SCADA::Logbook* G1Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(120U); } // G1 Overload
        static WarGrey::SCADA::Logbook* G1VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(121U); } // G1 Current Very High
        static WarGrey::SCADA::Logbook* G1PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(122U); } // G1 Power Module Temperature High
        static WarGrey::SCADA::Logbook* G1PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(123U); } // G1 Power Module Temperature Very High
        static WarGrey::SCADA::Logbook* G1Diesel2CMisc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(124U); } // G1 Diesel 2nd Class Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* G1UpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(125U); } // G1 U Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* G1VpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(126U); } // G1 V Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* G1WpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(127U); } // G1 W Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* G1DBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(128U); } // G1 Driven Bearing Temperature High
        static WarGrey::SCADA::Logbook* G1nDBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(129U); } // G1 non-Driven Bearing Temperature High
        static WarGrey::SCADA::Logbook* G1SPTransformerHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(130U); } // G1 Shore Power Transformer Temperature High
        static WarGrey::SCADA::Logbook* G1UpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(131U); } // G1 U Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* G1VpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(132U); } // G1 V Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* G1WpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(133U); } // G1 W Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* G1DBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(134U); } // G1 Driven Bearing Temperature Very High
        static WarGrey::SCADA::Logbook* G1nDBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(135U); } // G1 non-Driven Bearing Temperature Very High
        static WarGrey::SCADA::Logbook* G1SPTransformerVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(136U); } // G1 Shore Power Transformer Temperature Very High
        static WarGrey::SCADA::Logbook* G1SPCBOFF() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(137U); } // G1 Shore Power Circuit Breaker OFF
        static WarGrey::SCADA::Logbook* G1Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(138U); } // G1 Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* G1SPTransformerTSwitchFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(139U); } // G1 Shore Power Transformer Temperature Switch Fault
        static WarGrey::SCADA::Logbook* G2PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(140U); } // G2 Power Module Fault
        static WarGrey::SCADA::Logbook* G2IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(141U); } // G2 Instantaneous Over Current Relay Fault
        static WarGrey::SCADA::Logbook* G2DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(142U); } // G2 Definite Time Over Current Relay Fault
        static WarGrey::SCADA::Logbook* G2MotorOverSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(143U); } // G2 Motor Rotational Speed Exceed
        static WarGrey::SCADA::Logbook* G2MotorUnderSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(144U); } // G2 Motor Rotational Speed Beneath
        static WarGrey::SCADA::Logbook* G2Diesel1CMisc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(145U); } // G2 Diesel 1st Class Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* G2CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(146U); } // G2 CB1 Over Current Protective Relay Tripped Off
        static WarGrey::SCADA::Logbook* G2DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(147U); } // G2 DC Fuse Broken
        static WarGrey::SCADA::Logbook* G2UpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(148U); } // G2 U Phase Winding Temperature Exceed
        static WarGrey::SCADA::Logbook* G2VpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(149U); } // G2 V Phase Winding Temperature Exceed
        static WarGrey::SCADA::Logbook* G2WpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(150U); } // G2 W Phase Winding Temperature Exceed
        static WarGrey::SCADA::Logbook* G2DBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(151U); } // G2 Driven Bearing Temperature Exceed
        static WarGrey::SCADA::Logbook* G2nDBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(152U); } // G2 non-Driven Bearing Temperature Exceed
        static WarGrey::SCADA::Logbook* G2PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(153U); } // G2 Power Module Temperature Exceed
        static WarGrey::SCADA::Logbook* G2Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(154U); } // G2 Overload
        static WarGrey::SCADA::Logbook* G2VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(155U); } // G2 Current Very High
        static WarGrey::SCADA::Logbook* G2PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(156U); } // G2 Power Module Temperature High
        static WarGrey::SCADA::Logbook* G2PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(157U); } // G2 Power Module Temperature Very High
        static WarGrey::SCADA::Logbook* G2Diesel2CMisc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(158U); } // G2 Diesel 2nd Class Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* G2UpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(159U); } // G2 U Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* G2VpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(160U); } // G2 V Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* G2WpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(161U); } // G2 W Phase Winding Temperature High
        static WarGrey::SCADA::Logbook* G2DBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(162U); } // G2 Driven Bearing Temperature High
        static WarGrey::SCADA::Logbook* G2nDBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(163U); } // G2 non-Driven Bearing Temperature High
        static WarGrey::SCADA::Logbook* G2UpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(164U); } // G2 U Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* G2VpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(165U); } // G2 V Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* G2WpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(166U); } // G2 W Phase Winding Temperature Very High
        static WarGrey::SCADA::Logbook* G2DBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(167U); } // G2 Driven Bearing Temperature Very High
        static WarGrey::SCADA::Logbook* G2nDBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(168U); } // G2 non-Driven Bearing Temperature Very High
        static WarGrey::SCADA::Logbook* G2Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(169U); } // G2 Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* B1PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(170U); } // B1 Power Module Fault
        static WarGrey::SCADA::Logbook* B1IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(171U); } // B1 Instantaneous Over Current Relay Fault
        static WarGrey::SCADA::Logbook* B1DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(172U); } // B1 Definite Time Over Current Relay Fault
        static WarGrey::SCADA::Logbook* B1CB2TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(173U); } // B1 CB2 Over Current Protective Relay Tripped Off
        static WarGrey::SCADA::Logbook* B1DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(174U); } // B1 DC Fuse Broken
        static WarGrey::SCADA::Logbook* B1PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(175U); } // B1 Power Module Temperature Exceed
        static WarGrey::SCADA::Logbook* B1Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(176U); } // B1 Overload
        static WarGrey::SCADA::Logbook* B1VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(177U); } // B1 Current Very High
        static WarGrey::SCADA::Logbook* B1PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(178U); } // B1 Power Module Temperature High
        static WarGrey::SCADA::Logbook* B1PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(179U); } // B1 Power Module Temperature Very High
        static WarGrey::SCADA::Logbook* B1Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(180U); } // B1 Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* B1InductorTSwitchFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(181U); } // B1 Inductor Temperature Switch Fault
        static WarGrey::SCADA::Logbook* T1PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(185U); } // T1 Power Module Fault
        static WarGrey::SCADA::Logbook* T1IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(186U); } // T1 Instantaneous Over Current Relay Fault
        static WarGrey::SCADA::Logbook* T1DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(187U); } // T1 Definite Time Over Current Relay Fault
        static WarGrey::SCADA::Logbook* T1CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(188U); } // T1 CB1 Over Current Protective Relay Tripped Off
        static WarGrey::SCADA::Logbook* T1DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(189U); } // T1 DC Fuse Broken
        static WarGrey::SCADA::Logbook* T1PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(190U); } // T1 Power Module Temperature Exceed
        static WarGrey::SCADA::Logbook* T1Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(191U); } // T1 Overload
        static WarGrey::SCADA::Logbook* T1VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(192U); } // T1 Current Very High
        static WarGrey::SCADA::Logbook* T1PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(193U); } // T1 Power Module Temperature High
        static WarGrey::SCADA::Logbook* T1PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(194U); } // T1 Power Module Temperature Very High
        static WarGrey::SCADA::Logbook* T1Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(195U); } // T1 Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* T2PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(196U); } // T2 Power Module Fault
        static WarGrey::SCADA::Logbook* T2IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(197U); } // T2 Instantaneous Over Current Relay Fault
        static WarGrey::SCADA::Logbook* T2DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(198U); } // T2 Definite Time Over Current Relay Fault
        static WarGrey::SCADA::Logbook* T2CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(199U); } // T2 CB1 Over Current Protective Relay Tripped Off
        static WarGrey::SCADA::Logbook* T2DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(200U); } // T2 DC Fuse Broken
        static WarGrey::SCADA::Logbook* T2PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(201U); } // T2 Power Module Temperature Exceed
        static WarGrey::SCADA::Logbook* T2Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(202U); } // T2 Overload
        static WarGrey::SCADA::Logbook* T2VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(203U); } // T2 Current Very High
        static WarGrey::SCADA::Logbook* T2PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(204U); } // T2 Power Module Temperature High
        static WarGrey::SCADA::Logbook* T2PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(205U); } // T2 Power Module Temperature Very High
        static WarGrey::SCADA::Logbook* T2Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(206U); } // T2 Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* H1Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(217U); } // H1 Miscellaneous Alerting
        static WarGrey::SCADA::Logbook* H1CoolsrcPressureLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(218U); } // H1 Cooling Source Water Pressure signal lost
        static WarGrey::SCADA::Logbook* H1CoolsrcTemperatureLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(219U); } // H1 Cooling Source Water Temperature signal lost
        static WarGrey::SCADA::Logbook* H1CoolantPressureLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(220U); } // H1 Cooling Water Pressure signal lost
        static WarGrey::SCADA::Logbook* H1CoolantFlowLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(221U); } // H1 Cooling Water Volumetric Flow Rate signal lost
        static WarGrey::SCADA::Logbook* H1CoolantTemperatureLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(222U); } // H1 Cooling Water Temperature signal lost
        static WarGrey::SCADA::Logbook* H13WRValveLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(223U); } // H1 3-Way Regulating Valve signal lost
        static WarGrey::SCADA::Logbook* H1CoolantOverFlow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(224U); } // H1 Cooling Water Volumetric Flow Rate Exceed
        static WarGrey::SCADA::Logbook* H1CoolantHighFlow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(225U); } // H1 Cooling Water Volumetric Flow Rate High
        static WarGrey::SCADA::Logbook* H1CoolantUnderFlow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(226U); } // H1 Cooling Water Volumetric Flow Rate Beneath
        static WarGrey::SCADA::Logbook* H1CoolantLowFlow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(227U); } // H1 Cooling Water Volumetric Flow Rate Low
        static WarGrey::SCADA::Logbook* H1CoolsrcOverPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(228U); } // H1 Cooling Source Water Pressure Exceed
        static WarGrey::SCADA::Logbook* H1CoolsrcHighPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(229U); } // H1 Cooling Source Water Pressure High
        static WarGrey::SCADA::Logbook* H1CoolsrcUnderPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(230U); } // H1 Cooling Source Water Pressure Beneath
        static WarGrey::SCADA::Logbook* H1CoolsrcLowPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(231U); } // H1 Cooling Source Water Pressure Low
        static WarGrey::SCADA::Logbook* H1CoolantOverPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(232U); } // H1 Cooling Water Pressure Exceed
        static WarGrey::SCADA::Logbook* H1CoolantHighPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(233U); } // H1 Cooling Water Pressure High
        static WarGrey::SCADA::Logbook* H1CoolantUnderPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(234U); } // H1 Cooling Water Pressure Beneath
        static WarGrey::SCADA::Logbook* H1CoolantLowPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(235U); } // H1 Cooling Water Pressure Low
        static WarGrey::SCADA::Logbook* H1CoolsrcOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(236U); } // H1 Cooling Source Water Temperature Exceed
        static WarGrey::SCADA::Logbook* H1CoolsrcHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(237U); } // H1 Cooling Source Water Temperature High
        static WarGrey::SCADA::Logbook* H1CoolsrcUnderTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(238U); } // H1 Cooling Source Water Temperature Beneath
        static WarGrey::SCADA::Logbook* H1CoolsrcLowTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(239U); } // H1 Cooling Source Water Temperature Low
        static WarGrey::SCADA::Logbook* H1CoolantOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(240U); } // H1 Cooling Water Temperature Exceed
        static WarGrey::SCADA::Logbook* H1CoolantHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(241U); } // H1 Cooling Water Temperature High
        static WarGrey::SCADA::Logbook* H1CoolantUnderTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(242U); } // H1 Cooling Water Temperature Beneath
        static WarGrey::SCADA::Logbook* H1CoolantLowTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(243U); } // H1 Cooling Water Temperature Low
        static WarGrey::SCADA::Logbook* H13WRValveMismatch() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(244U); } // H1 3-Way Regulating Valve Actual State Mismatches the Feedback
        static WarGrey::SCADA::Logbook* H1CoolantUnderLoadTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(245U); } // H1 Cooling Water Temperature Beneath and Loading
        static WarGrey::SCADA::Logbook* H1220VPowerFailure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(246U); } // H1 220V Power Failure
        static WarGrey::SCADA::Logbook* G3PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(249U); } // G3 Power Module Fault
        static WarGrey::SCADA::Logbook* G3IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(250U); } // G3 Instantaneous Over Current Relay Fault
        static WarGrey::SCADA::Logbook* G3DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(251U); } // G3 Definite Time Over Current Relay Fault
        static WarGrey::SCADA::Logbook* G3DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(252U); } // G3 DC Fuse Broken
        static WarGrey::SCADA::Logbook* D1DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(253U); } // D1 DC Fuse Broken
        static WarGrey::SCADA::Logbook* G3PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(254U); } // G3 Power Module Temperature Exceed
        static WarGrey::SCADA::Logbook* G3Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(255U); } // G3 Overload
        static WarGrey::SCADA::Logbook* G3VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(256U); } // G3 Current Very High
        static WarGrey::SCADA::Logbook* G3PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(257U); } // G3 Power Module Temperature High
        static WarGrey::SCADA::Logbook* G3PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::Logbook>::UnsafeTongue(258U); } // G3 Power Module Temperature Very High

    protected:
        static size_t indices_size() { return 228U; }
        static const unsigned int* indices() {
            static const unsigned int indexes[] = {
                
