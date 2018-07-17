#pragma once

#include "tongue.hpp"

namespace WarGrey::SCADA {
    private class LogBook : public WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook> {
        friend class WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>;
    public:
        static Platform::String^ type() { return "LogBook"; }
        static WarGrey::SCADA::LogBook* fromIndex(unsigned int idx) { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::SafeTongue(idx);

    public:
        static WarGrey::SCADA::LogBook* FWCoolant1Running() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(1U); } // 1#冷却淡水泵运行
        static WarGrey::SCADA::LogBook* FWCoolant1Fault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(2U); } // 1#冷却淡水泵故障
        static WarGrey::SCADA::LogBook* FWCoolant2Running() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(3U); } // 2#冷却淡水泵运行
        static WarGrey::SCADA::LogBook* FWCoolant2Fault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(4U); } // 2#冷却淡水泵故障
        static WarGrey::SCADA::LogBook* SWCoolant1Running() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(5U); } // 1#冷却海水泵运行
        static WarGrey::SCADA::LogBook* SWCoolant1Fault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(6U); } // 1#冷却海水泵故障
        static WarGrey::SCADA::LogBook* SWCoolant2Running() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(7U); } // 2#冷却海水泵运行
        static WarGrey::SCADA::LogBook* SWCoolant2Fault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(8U); } // 2#冷却海水泵故障
        static WarGrey::SCADA::LogBook* DC24VBPLowResistance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(9U); } // DC24V配电板绝缘低
        static WarGrey::SCADA::LogBook* AC220VBPLowResistance() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(10U); } // AC220V配电板绝缘低
        static WarGrey::SCADA::LogBook* WaterWorksRunning() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(11U); } // 供水装置运行
        static WarGrey::SCADA::LogBook* WaterWorksFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(12U); } // 供水装置故障
        static WarGrey::SCADA::LogBook* ACCoolantRunning() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(17U); } // 空调冷却水泵运行
        static WarGrey::SCADA::LogBook* ACCoolantFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(18U); } // 空调冷却水泵故障
        static WarGrey::SCADA::LogBook* CO2PowerFailure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(19U); } // CO2失电报警
        static WarGrey::SCADA::LogBook* CO2Leak() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(20U); } // CO2泄露报警
        static WarGrey::SCADA::LogBook* CO2Release() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(21U); } // CO2释放报警
        static WarGrey::SCADA::LogBook* M1PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(25U); } // M1功率模块故障
        static WarGrey::SCADA::LogBook* PSBusOverVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(26U); } // 左舷直流电压超高故障
        static WarGrey::SCADA::LogBook* PSBusUnderVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(27U); } // 左舷直流电压超低故障
        static WarGrey::SCADA::LogBook* M1IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(28U); } // M1瞬时过流故障
        static WarGrey::SCADA::LogBook* M1DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(29U); } // M1延时过流故障
        static WarGrey::SCADA::LogBook* M1MotorOverSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(30U); } // M1超速故障
        static WarGrey::SCADA::LogBook* M1MotorUnderSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(31U); } // M1电机转速低故障
        static WarGrey::SCADA::LogBook* M1CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(32U); } // M1主断路器CB1过流脱扣
        static WarGrey::SCADA::LogBook* M1DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(33U); } // M1直流熔断器损坏
        static WarGrey::SCADA::LogBook* M1UpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(34U); } // M1 U相绕组温度超高故障
        static WarGrey::SCADA::LogBook* M1VpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(35U); } // M1 V相绕组温度超高故障
        static WarGrey::SCADA::LogBook* M1WpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(36U); } // M1 W相绕组温度超高故障
        static WarGrey::SCADA::LogBook* M1DBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(37U); } // M1驱动端轴承温度超高故障
        static WarGrey::SCADA::LogBook* M1nDBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(38U); } // M1非驱动端轴承温度超高故障
        static WarGrey::SCADA::LogBook* M1TBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(39U); } // M1艉轴温度超高故障
        static WarGrey::SCADA::LogBook* M1PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(40U); } // M1功率模块温度超高故障
        static WarGrey::SCADA::LogBook* PSBusHighVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(41U); } // 左舷直流电压高报警
        static WarGrey::SCADA::LogBook* PSBusLowVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(42U); } // 左舷直流电压低报警
        static WarGrey::SCADA::LogBook* M1Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(43U); } // M1过载报警
        static WarGrey::SCADA::LogBook* M1VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(44U); } // M1电流较高报警
        static WarGrey::SCADA::LogBook* M1PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(45U); } // M1功率模块温度高报警
        static WarGrey::SCADA::LogBook* M1PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(46U); } // M1功率模块温度较高报警
        static WarGrey::SCADA::LogBook* M1MotorLocked() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(47U); } // M1电机处于锁轴状态
        static WarGrey::SCADA::LogBook* M1UpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(48U); } // M1 U相绕组温度高报警
        static WarGrey::SCADA::LogBook* M1VpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(49U); } // M1 V相绕组温度高报警
        static WarGrey::SCADA::LogBook* M1WpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(50U); } // M1 W相绕组温度高报警
        static WarGrey::SCADA::LogBook* M1DBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(51U); } // M1驱动端轴承温度高报警
        static WarGrey::SCADA::LogBook* M1nDBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(52U); } // M1非驱动端轴承温度高报警
        static WarGrey::SCADA::LogBook* M1TBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(53U); } // M1艉轴温度高报警
        static WarGrey::SCADA::LogBook* M1UpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(54U); } // M1 U相绕组温度较高报警
        static WarGrey::SCADA::LogBook* M1VpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(55U); } // M1 V相绕组温度较高报警
        static WarGrey::SCADA::LogBook* M1WpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(56U); } // M1 W相绕组温度较高报警
        static WarGrey::SCADA::LogBook* M1DBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(57U); } // M1驱动端轴承温度较高报警
        static WarGrey::SCADA::LogBook* M1nDBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(58U); } // M1非驱动端轴承温度较高报警
        static WarGrey::SCADA::LogBook* M1TBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(59U); } // M1艉轴温度较高报警
        static WarGrey::SCADA::LogBook* M1Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(60U); } // M1其他综合报警
        static WarGrey::SCADA::LogBook* M2PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(61U); } // M2功率模块故障
        static WarGrey::SCADA::LogBook* SBBusOverVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(62U); } // 右舷直流电压超高故障
        static WarGrey::SCADA::LogBook* SBBusUnderVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(63U); } // 右舷直流电压超低故障
        static WarGrey::SCADA::LogBook* M2IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(64U); } // M2瞬时过流故障
        static WarGrey::SCADA::LogBook* M2DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(65U); } // M2延时过流故障
        static WarGrey::SCADA::LogBook* M2MotorOverSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(66U); } // M2超速故障
        static WarGrey::SCADA::LogBook* M2MotorUnderSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(67U); } // M2电机转速低故障
        static WarGrey::SCADA::LogBook* M2CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(68U); } // M2主断路器CB1过流脱扣
        static WarGrey::SCADA::LogBook* M2DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(69U); } // M2直流熔断器损坏
        static WarGrey::SCADA::LogBook* M2UOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(70U); } // M2 U相绕组温度超高故障
        static WarGrey::SCADA::LogBook* M2VOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(71U); } // M2 V相绕组温度超高故障
        static WarGrey::SCADA::LogBook* M2WOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(72U); } // M2 W相绕组温度超高故障
        static WarGrey::SCADA::LogBook* M2DBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(73U); } // M2驱动端轴承温度超高故障
        static WarGrey::SCADA::LogBook* M2nDBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(74U); } // M2非驱动端轴承温度超高故障
        static WarGrey::SCADA::LogBook* M2TBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(75U); } // M2艉轴温度超高故障
        static WarGrey::SCADA::LogBook* M2PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(76U); } // M2功率模块温度超高故障
        static WarGrey::SCADA::LogBook* SBBusHighVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(77U); } // 右舷直流电压高报警
        static WarGrey::SCADA::LogBook* SBBusLowVoltage() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(78U); } // 右舷直流电压低报警
        static WarGrey::SCADA::LogBook* M2Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(79U); } // M2过载报警
        static WarGrey::SCADA::LogBook* M2VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(80U); } // M2电流较高报警
        static WarGrey::SCADA::LogBook* M2PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(81U); } // M2功率模块温度高报警
        static WarGrey::SCADA::LogBook* M2PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(82U); } // M2功率模块温度较高报警
        static WarGrey::SCADA::LogBook* M2MotorLocked() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(83U); } // M2电机处于锁轴状态
        static WarGrey::SCADA::LogBook* M2UpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(84U); } // M2 U相绕组温度高报警
        static WarGrey::SCADA::LogBook* M2VpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(85U); } // M2 V相绕组温度高报警
        static WarGrey::SCADA::LogBook* M2WpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(86U); } // M2 W相绕组温度高报警
        static WarGrey::SCADA::LogBook* M2DBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(87U); } // M2驱动端轴承温度高报警
        static WarGrey::SCADA::LogBook* M2nDBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(88U); } // M2非驱动端轴承温度高报警
        static WarGrey::SCADA::LogBook* M2TBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(89U); } // M2艉轴温度高报警
        static WarGrey::SCADA::LogBook* M2UpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(90U); } // M2 U相绕组温度较高报警
        static WarGrey::SCADA::LogBook* M2VpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(91U); } // M2 V相绕组温度较高报警
        static WarGrey::SCADA::LogBook* M2WpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(92U); } // M2 W相绕组温度较高报警
        static WarGrey::SCADA::LogBook* M2DBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(93U); } // M2驱动端轴承温度较高报警
        static WarGrey::SCADA::LogBook* M2nDBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(94U); } // M2非驱动端轴承温度较高报警
        static WarGrey::SCADA::LogBook* M2TBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(95U); } // M2艉轴温度较高报警
        static WarGrey::SCADA::LogBook* M2Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(96U); } // M2其他综合报警
        static WarGrey::SCADA::LogBook* G1PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(105U); } // G1功率模块故障
        static WarGrey::SCADA::LogBook* G1IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(106U); } // G1瞬时过流故障
        static WarGrey::SCADA::LogBook* G1DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(107U); } // G1延时过流故障
        static WarGrey::SCADA::LogBook* G1MotorOverSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(108U); } // G1超速故障
        static WarGrey::SCADA::LogBook* G1MotorUnderSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(109U); } // G1电机转速低故障
        static WarGrey::SCADA::LogBook* G1Diesel1CMisc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(110U); } // G1柴油机一类综合故障
        static WarGrey::SCADA::LogBook* G1CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(111U); } // G1主断路器CB1过流脱扣
        static WarGrey::SCADA::LogBook* G1DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(112U); } // G1直流熔断器损坏
        static WarGrey::SCADA::LogBook* G1UpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(113U); } // G1 U相绕组温度超高故障
        static WarGrey::SCADA::LogBook* G1VpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(114U); } // G1 V相绕组温度超高故障
        static WarGrey::SCADA::LogBook* G1WpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(115U); } // G1 W相绕组温度超高故障
        static WarGrey::SCADA::LogBook* G1DBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(116U); } // G1驱动端轴承温度超高故障
        static WarGrey::SCADA::LogBook* G1nDBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(117U); } // G1非驱动端轴承温度超高故障
        static WarGrey::SCADA::LogBook* G1SPTransformerOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(118U); } // G1岸电电源变压器温度超高故障
        static WarGrey::SCADA::LogBook* G1PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(119U); } // G1功率模块温度超高故障
        static WarGrey::SCADA::LogBook* G1Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(120U); } // G1过载报警
        static WarGrey::SCADA::LogBook* G1VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(121U); } // G1电流较高报警
        static WarGrey::SCADA::LogBook* G1PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(122U); } // G1功率模块温度高报警
        static WarGrey::SCADA::LogBook* G1PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(123U); } // G1功率模块温度较高报警
        static WarGrey::SCADA::LogBook* G1Diesel2CMisc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(124U); } // G1柴油机二类综合故障
        static WarGrey::SCADA::LogBook* G1UpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(125U); } // G1 U相绕组温度高报警
        static WarGrey::SCADA::LogBook* G1VpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(126U); } // G1 V相绕组温度高报警
        static WarGrey::SCADA::LogBook* G1WpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(127U); } // G1 W相绕组温度高报警
        static WarGrey::SCADA::LogBook* G1DBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(128U); } // G1驱动端轴承温度高报警
        static WarGrey::SCADA::LogBook* G1nDBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(129U); } // G1非驱动端轴承温度高报警
        static WarGrey::SCADA::LogBook* G1SPTransformerHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(130U); } // G1岸电电源变压器温度高报警
        static WarGrey::SCADA::LogBook* G1UpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(131U); } // G1 U相绕组温度较高报警
        static WarGrey::SCADA::LogBook* G1VpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(132U); } // G1 V相绕组温度较高报警
        static WarGrey::SCADA::LogBook* G1WpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(133U); } // G1 W相绕组温度较高报警
        static WarGrey::SCADA::LogBook* G1DBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(134U); } // G1驱动端轴承温度较高报警
        static WarGrey::SCADA::LogBook* G1nDBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(135U); } // G1非驱动端轴承温度较高报警
        static WarGrey::SCADA::LogBook* G1SPTransformerVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(136U); } // G1岸电电源变压器温度较高报警
        static WarGrey::SCADA::LogBook* G1SPCBOFF() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(137U); } // G1岸电主断路器断开
        static WarGrey::SCADA::LogBook* G1Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(138U); } // G1其他综合报警
        static WarGrey::SCADA::LogBook* G1SPTransformerTSwitchFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(139U); } // G1岸电变压器温度开关故障
        static WarGrey::SCADA::LogBook* G2PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(140U); } // G2功率模块故障
        static WarGrey::SCADA::LogBook* G2IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(141U); } // G2瞬时过流故障
        static WarGrey::SCADA::LogBook* G2DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(142U); } // G2延时过流故障
        static WarGrey::SCADA::LogBook* G2MotorOverSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(143U); } // G2超速故障
        static WarGrey::SCADA::LogBook* G2MotorUnderSpeed() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(144U); } // G2电机转速低故障
        static WarGrey::SCADA::LogBook* G2Diesel1CMisc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(145U); } // G2柴油机一类综合故障
        static WarGrey::SCADA::LogBook* G2CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(146U); } // G2主断路器CB1过流脱扣
        static WarGrey::SCADA::LogBook* G2DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(147U); } // G2直流熔断器损坏
        static WarGrey::SCADA::LogBook* G2UpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(148U); } // G2 U相绕组温度超高故障
        static WarGrey::SCADA::LogBook* G2VpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(149U); } // G2 V相绕组温度超高故障
        static WarGrey::SCADA::LogBook* G2WpwOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(150U); } // G2 W相绕组温度超高故障
        static WarGrey::SCADA::LogBook* G2DBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(151U); } // G2驱动端轴承温度超高故障
        static WarGrey::SCADA::LogBook* G2nDBOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(152U); } // G2非驱动端轴承温度超高故障
        static WarGrey::SCADA::LogBook* G2PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(153U); } // G2功率模块温度超高故障
        static WarGrey::SCADA::LogBook* G2Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(154U); } // G2过载报警
        static WarGrey::SCADA::LogBook* G2VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(155U); } // G2电流较高报警
        static WarGrey::SCADA::LogBook* G2PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(156U); } // G2功率模块温度高报警
        static WarGrey::SCADA::LogBook* G2PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(157U); } // G2功率模块温度较高报警
        static WarGrey::SCADA::LogBook* G2Diesel2CMisc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(158U); } // G2柴油机二类综合故障
        static WarGrey::SCADA::LogBook* G2UpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(159U); } // G2 U相绕组温度高报警
        static WarGrey::SCADA::LogBook* G2VpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(160U); } // G2 V相绕组温度高报警
        static WarGrey::SCADA::LogBook* G2WpwHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(161U); } // G2 W相绕组温度高报警
        static WarGrey::SCADA::LogBook* G2DBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(162U); } // G2驱动端轴承温度高报警
        static WarGrey::SCADA::LogBook* G2nDBHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(163U); } // G2非驱动端轴承温度高报警
        static WarGrey::SCADA::LogBook* G2UpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(164U); } // G2 U相绕组温度较高报警
        static WarGrey::SCADA::LogBook* G2VpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(165U); } // G2 V相绕组温度较高报警
        static WarGrey::SCADA::LogBook* G2WpwVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(166U); } // G2 W相绕组温度较高报警
        static WarGrey::SCADA::LogBook* G2DBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(167U); } // G2驱动端轴承温度较高报警
        static WarGrey::SCADA::LogBook* G2nDBVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(168U); } // G2非驱动端轴承温度较高报警
        static WarGrey::SCADA::LogBook* G2Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(169U); } // G2其他综合报警
        static WarGrey::SCADA::LogBook* B1PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(170U); } // B1功率模块故障
        static WarGrey::SCADA::LogBook* B1IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(171U); } // B1瞬时过流故障
        static WarGrey::SCADA::LogBook* B1DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(172U); } // B1延时过流故障
        static WarGrey::SCADA::LogBook* B1CB2TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(173U); } // B1主断路器CB2过流脱扣
        static WarGrey::SCADA::LogBook* B1DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(174U); } // B1直流熔断器损坏
        static WarGrey::SCADA::LogBook* B1PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(175U); } // B1功率模块温度超高故障
        static WarGrey::SCADA::LogBook* B1Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(176U); } // B1过载报警
        static WarGrey::SCADA::LogBook* B1VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(177U); } // B1电流较高报警
        static WarGrey::SCADA::LogBook* B1PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(178U); } // B1功率模块温度高报警
        static WarGrey::SCADA::LogBook* B1PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(179U); } // B1功率模块温度较高报警
        static WarGrey::SCADA::LogBook* B1Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(180U); } // B1其他综合报警
        static WarGrey::SCADA::LogBook* B1InductorTSwitchFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(181U); } // B1电抗器温度开关故障
        static WarGrey::SCADA::LogBook* T1PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(185U); } // T1功率模块故障
        static WarGrey::SCADA::LogBook* T1IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(186U); } // T1瞬时过流故障
        static WarGrey::SCADA::LogBook* T1DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(187U); } // T1延时过流故障
        static WarGrey::SCADA::LogBook* T1CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(188U); } // T1主断路器CB1过流脱扣
        static WarGrey::SCADA::LogBook* T1DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(189U); } // T1直流熔断器损坏
        static WarGrey::SCADA::LogBook* T1PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(190U); } // T1功率模块温度超高故障
        static WarGrey::SCADA::LogBook* T1Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(191U); } // T1过载报警
        static WarGrey::SCADA::LogBook* T1VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(192U); } // T1电流较高报警
        static WarGrey::SCADA::LogBook* T1PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(193U); } // T1功率模块温度高报警
        static WarGrey::SCADA::LogBook* T1PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(194U); } // T1功率模块温度较高报警
        static WarGrey::SCADA::LogBook* T1Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(195U); } // T1其他综合报警
        static WarGrey::SCADA::LogBook* T2PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(196U); } // T2功率模块故障
        static WarGrey::SCADA::LogBook* T2IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(197U); } // T2瞬时过流故障
        static WarGrey::SCADA::LogBook* T2DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(198U); } // T2延时过流故障
        static WarGrey::SCADA::LogBook* T2CB1TripOff() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(199U); } // T2主断路器CB1过流脱扣
        static WarGrey::SCADA::LogBook* T2DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(200U); } // T2直流熔断器损坏
        static WarGrey::SCADA::LogBook* T2PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(201U); } // T2功率模块温度超高故障
        static WarGrey::SCADA::LogBook* T2Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(202U); } // T2过载报警
        static WarGrey::SCADA::LogBook* T2VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(203U); } // T2电流较高报警
        static WarGrey::SCADA::LogBook* T2PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(204U); } // T2功率模块温度高报警
        static WarGrey::SCADA::LogBook* T2PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(205U); } // T2功率模块温度较高报警
        static WarGrey::SCADA::LogBook* T2Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(206U); } // T2其他综合报警
        static WarGrey::SCADA::LogBook* H1Misc() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(217U); } // H1其他综合报警
        static WarGrey::SCADA::LogBook* H1CoolsrcPressureLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(218U); } // H1冷源水压力信号已断开
        static WarGrey::SCADA::LogBook* H1CoolsrcTemperatureLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(219U); } // H1冷源水温度信号已断开
        static WarGrey::SCADA::LogBook* H1CoolantPressureLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(220U); } // H1冷却水压力信号已断开
        static WarGrey::SCADA::LogBook* H1CoolantFlowLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(221U); } // H1冷却水流量信号已断开
        static WarGrey::SCADA::LogBook* H1CoolantTemperatureLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(222U); } // H1冷却水温度信号已断开
        static WarGrey::SCADA::LogBook* H13WRValveLost() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(223U); } // H1三通阀信号已断开
        static WarGrey::SCADA::LogBook* H1CoolantOverFlow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(224U); } // H1冷却水流量超高报警
        static WarGrey::SCADA::LogBook* H1CoolantHighFlow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(225U); } // H1冷却水流量高报警
        static WarGrey::SCADA::LogBook* H1CoolantUnderFlow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(226U); } // H1冷却水流量超低报警
        static WarGrey::SCADA::LogBook* H1CoolantLowFlow() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(227U); } // H1冷却水流量低报警
        static WarGrey::SCADA::LogBook* H1CoolsrcOverPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(228U); } // H1冷源水压力超高报警
        static WarGrey::SCADA::LogBook* H1CoolsrcHighPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(229U); } // H1冷源水压力高报警
        static WarGrey::SCADA::LogBook* H1CoolsrcUnderPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(230U); } // H1冷源水压力超低报警
        static WarGrey::SCADA::LogBook* H1CoolsrcLowPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(231U); } // H1冷源水压力低报警
        static WarGrey::SCADA::LogBook* H1CoolantOverPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(232U); } // H1冷却水压力超高报警
        static WarGrey::SCADA::LogBook* H1CoolantHighPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(233U); } // H1冷却水压力高报警
        static WarGrey::SCADA::LogBook* H1CoolantUnderPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(234U); } // H1冷却水压力超低报警
        static WarGrey::SCADA::LogBook* H1CoolantLowPressure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(235U); } // H1冷却水压力低报警
        static WarGrey::SCADA::LogBook* H1CoolsrcOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(236U); } // H1冷源水温度超高报警
        static WarGrey::SCADA::LogBook* H1CoolsrcHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(237U); } // H1冷源水温度高报警
        static WarGrey::SCADA::LogBook* H1CoolsrcUnderTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(238U); } // H1冷源水温度超低报警
        static WarGrey::SCADA::LogBook* H1CoolsrcLowTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(239U); } // H1冷源水温度低报警
        static WarGrey::SCADA::LogBook* H1CoolantOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(240U); } // H1冷却水温度超高报警
        static WarGrey::SCADA::LogBook* H1CoolantHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(241U); } // H1冷却水温度高报警
        static WarGrey::SCADA::LogBook* H1CoolantUnderTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(242U); } // H1冷却水温度超低报警
        static WarGrey::SCADA::LogBook* H1CoolantLowTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(243U); } // H1冷却水温度低报警
        static WarGrey::SCADA::LogBook* H13WRValveMismatch() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(244U); } // H1三通阀位置给定与反馈不等报警
        static WarGrey::SCADA::LogBook* H1CoolantUnderLoadTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(245U); } // H1冷却水温度超低且有泵运行报警
        static WarGrey::SCADA::LogBook* H1220VPowerFailure() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(246U); } // H1 220V供电丢失
        static WarGrey::SCADA::LogBook* G3PMFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(249U); } // G3功率模块故障
        static WarGrey::SCADA::LogBook* G3IOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(250U); } // G3瞬时过流故障
        static WarGrey::SCADA::LogBook* G3DTOCFault() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(251U); } // G3延时过流故障
        static WarGrey::SCADA::LogBook* G3DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(252U); } // G3直流熔断器损坏
        static WarGrey::SCADA::LogBook* D1DCFuseBroken() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(253U); } // D1直流熔断器损坏
        static WarGrey::SCADA::LogBook* G3PMOverTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(254U); } // G3功率模块温度超高故障
        static WarGrey::SCADA::LogBook* G3Overload() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(255U); } // G3过载报警
        static WarGrey::SCADA::LogBook* G3VeryHighCurrent() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(256U); } // G3电流较高报警
        static WarGrey::SCADA::LogBook* G3PMHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(257U); } // G3功率模块温度高报警
        static WarGrey::SCADA::LogBook* G3PMVeryHighTemperature() { return WarGrey::SCADA::Tongue<WarGrey::SCADA::LogBook>::UnsafeTongue(258U); } // G3功率模块温度较高报警

    public:
        unsigned int min_index() override { return 1U; }
        unsigned int max_index() override { return 800U; }

    private:
        LogBook(unsigned int idx) : Tongue(idx) {}
    };
}

