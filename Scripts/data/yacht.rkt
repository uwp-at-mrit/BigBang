#lang racket/gui

(require "csv.rkt")

(struct aevent (enum map en_US zh_CN) #:transparent)

(define src.csv (build-path (find-system-path 'desk-dir) "data.csv"))
(define metrics (read-csv src.csv))

(define identify
  (let ([&lineno (box 1)])
    (lambda [zh_CN]
      (define id
        (case zh_CN
          [("1#冷却淡水泵运行") (cons 'WaterCP1Running "Fresh Water Cooling Pump#1 is Running")] ;;; line 1
          [("1#冷却淡水泵故障") (cons 'WaterCP1Broken "Fresh Water Cooling Pump#1 is Broken")]  ;;; line 2
          [("2#冷却淡水泵运行") (cons 'WaterCP2Running "Fresh Water Cooling Pump#2 is Running")] ;;; line 3
          [("2#冷却淡水泵故障") (cons 'WaterCP2Broken "Fresh Water Cooling Pump#2 is Broken")] ;;; line 4
          [("1#冷却海水泵运行") (cons 'SeaCP1Running "Sea Water Cooling Pump#1 is Running")] ;;; line 5
          [("1#冷却海水泵故障") (cons 'SeaCP1Broken "Sea Water Cooling Pump#1 is Broken")] ;;; line 6
          [("2#冷却海水泵运行") (cons 'SeaCP2Running "Sea Water Cooling Pump#2 is Running")] ;;; line 7
          [("2#冷却海水泵故障") (cons 'SeaCP2Broken "Sea Water Cooling Pump#2 is Broken")] ;;; line 8
          [("DC24V配电板绝缘低") (cons 'DCPanel24V "DC24V Breaker Panel Abnormal")] ;;; line 9
          [("AC220V配电板绝缘低") (cons 'ACPanel220V "AC220V Breaker Panel Abnormal")] ;;; line 10
          [("供水装置运行") (cons 'WaterSDRunning "Water Supply Device is Running")] ;;; line 11
          [("供水装置故障") (cons 'WaterSDBroken "Water Supply Device is Broken")] ;;; line 12
          [("空调冷却水泵运行") (cons 'ACCPRunning "AC Cooling Pump is Running")] ;;; line 17
          [("空调冷却水泵故障") (cons 'ACCPBroken "AC Cooling Pump is Broken")] ;;; line 18
          [("CO2失电报警") (cons 'CO2Power "CO2 Device Power Failure")] ;;; line 19
          [("CO2泄露报警") (cons 'CO2Leak "CO2 is Leaking")] ;;; line 20
          [("CO2释放报警") (cons 'CO2Release "CO2 is Releasing")] ;;; line 21
          [("M1功率模块故障") (cons 'M1PowerBroken "M1 Power Module is Broken")] ;;; line 25
          [("左舷直流电压超高故障") (cons 'PSDCOverV "Port Station DC Voltage Exceed")] ;;; line 26
          [("左舷直流电压超低故障") (cons 'PSDCBeneathV "Port Station DC Voltage Not Enough")] ;;; line 27
          [("M1瞬时过流故障") (cons 'M1IOverCurrent "M1 Instantaneous Over Current Protective Relay is Broken")] ;;; line 28
          [("M1延时过流故障") (cons 'M1DTOverCurrent "M1 Definite Time Over Current Protective Relay is Broken")] ;;; line 29
          [("M1超速故障") (cons 'M1OverSpeed "M1 Motor Rounded Speed Exceed")] ;;; line 30
          [("M1电机转速低故障") (cons 'M1LackSpeed "M1 Motor Rounded Speed Not Enough")] ;;; line 31
          [("M1主断路器CB1过流脱扣") (cons 'M1CB1DropOff "M1 CB1 Over Current Relay has Dropped Off")] ;;; line 32
          [("M1直流熔断器损坏") (cons 'M1DCFuseBox "M1 DC Fuse Box is Broken")] ;;; line 33
          [("M1 U相绕组温度超高故障") (cons 'M1UpwOverT "M1 U Phase Winding Over Temperature")] ;;; line 34
          [("M1 V相绕组温度超高故障") (cons 'M1VpwOverT "M1 V Phase Winding Over Temperature")] ;;; line 35
          [("M1 W相绕组温度超高故障") (cons 'M1WpwOverT "M1 W Phase Winding Over Temperature")] ;;; line 36
          [("M1驱动端轴承温度超高故障") (cons 'M1DBOverT "M1 Driven Bearing Temperature Exceed")] ;;; line 37
          [("M1非驱动端轴承温度超高故障") (cons 'M1nDBOverT "M1 non-Driven Bearing Temperature Exceed")] ;;; line 38
          [("M1艉轴温度超高故障") (cons 'M1TBOverT "M1 Thrust Bearing Temperature Exceed")] ;;; line 39
          [("M1功率模块温度超高故障") (cons 'M1PMOverT "M1 Power Module Temperature Exceed")] ;;; line 40
          [("左舷直流电压高报警") (cons 'PSDCHighV "Port Station DC Voltage is High")] ;;; line 41
          [("左舷直流电压低报警") (cons 'PSDCLowV "Port Station DC Voltage is Low")] ;;; line 42
          [("M1过载报警") (cons 'M1Overload "M1 is Overloading")] ;;; line 43
          [("M1电流较高报警") (cons 'M1Current "M1 Current is High")] ;;; line 44
          [("M1功率模块温度高报警") (cons 'M1PowerHighT "M1 Power Module Temperature is High")] ;;; line 45
          [("M1功率模块温度较高报警") (cons 'M1PowerVeryHighT "M1 Power Module Temperature is Very High")] ;;; line 46
          [("M1电机处于锁轴状态") (cons 'M1MotorLocked "M1 Motor is Locked")] ;;; line 47
          [("M1 U相绕组温度高报警") (cons 'M1UpwHighT "M1 U Phase Winding Temperature is High")] ;;; line 48
          [("M1 V相绕组温度高报警") (cons 'M1VpwHighT "M1 V Phase Winding Temperature is High")] ;;; line 49
          [("M1 W相绕组温度高报警") (cons 'M1WpwHighT "M1 W Phase Winding Temperature is High")] ;;; line 50
          [("M1驱动端轴承温度高报警") (cons 'M1DBHighT "M1 Driven Bearing Temperature is High")] ;;; line 51
          [("M1非驱动端轴承温度高报警") (cons 'M1nDBHighT "M1 non-Driven Bearing Temperature is High")] ;;; line 52
          [("M1艉轴温度高报警") (cons 'M1TBHighT "M1 Thrust Bearing Temperature is High")] ;;; line 53
          [("M1 U相绕组温度较高报警") (cons 'M1UpwVeryHighT "M1 U Phase Winding Temperature is Very High")] ;;; line 54
          [("M1 V相绕组温度较高报警") (cons 'M1VpwVeryHighT "M1 U Phase Winding Temperature is Very High")] ;;; line 55
          [("M1 W相绕组温度较高报警") (cons 'M1WpwVeryHighT "M1 U Phase Winding Temperature is Very High")] ;;; line 56
          [("M1驱动端轴承温度较高报警") (cons 'M1DBVeryHighT "M1 Driven Bearing Temperature is Very High")] ;;; line 57
          [("M1非驱动端轴承温度较高报警") (cons 'M1nDBVeryHighT "M1 non-Driven Bearing Temperature is Very High")] ;;; line 58
          [("M1艉轴温度较高报警") (cons 'MTBVeryHighT "M1 Thrust Bearing Temperature is Very High")] ;;; line 59
          [("M1其他综合报警") (cons 'M1Misc "M1 Miscellaneous Alerting")] ;;; line 60
          [("M2功率模块故障") (cons 'M2PowerBroken "M2 Power Module Broken")] ;;; line 61
          [("右舷直流电压超高故障") (cons 'reversed "Reversed")] ;;; line 62
          [("右舷直流电压超低故障") (cons 'reversed "Reversed")] ;;; line 63
          [("M2瞬时过流故障") (cons 'reversed "Reversed")] ;;; line 64
          [("M2延时过流故障") (cons 'reversed "Reversed")] ;;; line 65
          [("M2超速故障") (cons 'reversed "Reversed")] ;;; line 66
          [("M2电机转速低故障") (cons 'reversed "Reversed")] ;;; line 67
          [("M2主断路器CB1过流脱扣") (cons 'reversed "Reversed")] ;;; line 68
          [("M2直流熔断器损坏") (cons 'reversed "Reversed")] ;;; line 69
          [("M2 U相绕组温度超高故障") (cons 'reversed "Reversed")] ;;; line 70
          [("M2 V相绕组温度超高故障") (cons 'reversed "Reversed")] ;;; line 71
          [("M2 W相绕组温度超高故障") (cons 'reversed "Reversed")] ;;; line 72
          [("M2驱动端轴承温度超高故障") (cons 'reversed "Reversed")] ;;; line 73
          [("M2非驱动端轴承温度超高故障") (cons 'reversed "Reversed")] ;;; line 74
          [("M2艉轴温度超高故障") (cons 'reversed "Reversed")] ;;; line 75
          [("M2功率模块温度超高故障") (cons 'reversed "Reversed")] ;;; line 76
          [("右舷直流电压高报警") (cons 'reversed "Reversed")] ;;; line 77
          [("右舷直流电压低报警") (cons 'reversed "Reversed")] ;;; line 78
          [("M2过载报警") (cons 'reversed "Reversed")] ;;; line 79
          [("M2电流较高报警") (cons 'reversed "Reversed")] ;;; line 80
          [("M2功率模块温度高报警") (cons 'reversed "Reversed")] ;;; line 81
          [("M2功率模块温度较高报警") (cons 'reversed "Reversed")] ;;; line 82
          [("M2电机处于锁轴状态") (cons 'reversed "Reversed")] ;;; line 83
          [("M2 U相绕组温度高报警") (cons 'reversed "Reversed")] ;;; line 84
          [("M2 V相绕组温度高报警") (cons 'reversed "Reversed")] ;;; line 85
          [("M2 W相绕组温度高报警") (cons 'reversed "Reversed")] ;;; line 86
          [("M2驱动端轴承温度高报警") (cons 'reversed "Reversed")] ;;; line 87
          [("M2非驱动端轴承温度高报警") (cons 'reversed "Reversed")] ;;; line 88
          [("M2艉轴温度高报警") (cons 'reversed "Reversed")] ;;; line 89
          [("M2 U相绕组温度较高报警") (cons 'reversed "Reversed")] ;;; line 90
          [("M2 V相绕组温度较高报警") (cons 'reversed "Reversed")] ;;; line 91
          [("M2 W相绕组温度较高报警") (cons 'reversed "Reversed")] ;;; line 92
          [("M2驱动端轴承温度较高报警") (cons 'reversed "Reversed")] ;;; line 93
          [("M2非驱动端轴承温度较高报警") (cons 'reversed "Reversed")] ;;; line 94
          [("M2艉轴温度较高报警") (cons 'reversed "Reversed")] ;;; line 95
          [("M2其他综合报警") (cons 'reversed "Reversed")] ;;; line 96
          [("G1功率模块故障") (cons 'reversed "Reversed")] ;;; line 105
          [("G1瞬时过流故障") (cons 'reversed "Reversed")] ;;; line 106
          [("G1延时过流故障") (cons 'reversed "Reversed")] ;;; line 107
          [("G1超速故障") (cons 'reversed "Reversed")] ;;; line 108
          [("G1电机转速低故障") (cons 'reversed "Reversed")] ;;; line 109
          [("G1柴油机一类综合故障") (cons 'reversed "Reversed")] ;;; line 110
          [("G1主断路器CB1过流脱扣") (cons 'reversed "Reversed")] ;;; line 111
          [("G1直流熔断器损坏") (cons 'reversed "Reversed")] ;;; line 112
          [("G1 U相绕组温度超高故障") (cons 'reversed "Reversed")] ;;; line 113
          [("G1 V相绕组温度超高故障") (cons 'reversed "Reversed")] ;;; line 114
          [("G1 W相绕组温度超高故障") (cons 'reversed "Reversed")] ;;; line 115
          [("G1驱动端轴承温度超高故障") (cons 'reversed "Reversed")] ;;; line 116
          [("G1非驱动端轴承温度超高故障") (cons 'reversed "Reversed")] ;;; line 117
          [("G1岸电电源变压器温度超高故障") (cons 'reversed "Reversed")] ;;; line 118
          [("G1功率模块温度超高故障") (cons 'reversed "Reversed")] ;;; line 119
          [("G1过载报警") (cons 'reversed "Reversed")] ;;; line 120
          [("G1电流较高报警") (cons 'reversed "Reversed")] ;;; line 121
          [("G1功率模块温度高报警") (cons 'reversed "Reversed")] ;;; line 122
          [("G1功率模块温度较高报警") (cons 'reversed "Reversed")] ;;; line 123
          [("G1柴油机二类综合故障") (cons 'reversed "Reversed")] ;;; line 124
          [("G1 U相绕组温度高报警") (cons 'reversed "Reversed")] ;;; line 125
          [("G1 V相绕组温度高报警") (cons 'reversed "Reversed")] ;;; line 126
          [("G1 W相绕组温度高报警") (cons 'reversed "Reversed")] ;;; line 127
          [("G1驱动端轴承温度高报警") (cons 'reversed "Reversed")] ;;; line 128
          [("G1非驱动端轴承温度高报警") (cons 'reversed "Reversed")] ;;; line 129
          [("G1岸电电源变压器温度高报警") (cons 'reversed "Reversed")] ;;; line 130
          [("G1 U相绕组温度较高报警") (cons 'reversed "Reversed")] ;;; line 131
          [("G1 V相绕组温度较高报警") (cons 'reversed "Reversed")] ;;; line 132
          [("G1 W相绕组温度较高报警") (cons 'reversed "Reversed")] ;;; line 133
          [("G1驱动端轴承温度较高报警") (cons 'reversed "Reversed")] ;;; line 134
          [("G1非驱动端轴承温度较高报警") (cons 'reversed "Reversed")] ;;; line 135
          [("G1岸电电源变压器温度较高报警") (cons 'reversed "Reversed")] ;;; line 136
          [("G1岸电主断路器断开") (cons 'reversed "Reversed")] ;;; line 137
          [("G1其他综合报警") (cons 'reversed "Reversed")] ;;; line 138
          [("G1岸电变压器温度开关故障") (cons 'reversed "Reversed")] ;;; line 139
          [("G2功率模块故障") (cons 'reversed "Reversed")] ;;; line 140
          [("G2瞬时过流故障") (cons 'reversed "Reversed")] ;;; line 141
          [("G2延时过流故障") (cons 'reversed "Reversed")] ;;; line 142
          [("G2超速故障") (cons 'reversed "Reversed")] ;;; line 143
          [("G2电机转速低故障") (cons 'reversed "Reversed")] ;;; line 144
          [("G2柴油机一类综合故障") (cons 'reversed "Reversed")] ;;; line 145
          [("G2主断路器CB1过流脱扣") (cons 'reversed "Reversed")] ;;; line 146
          [("G2直流熔断器损坏") (cons 'reversed "Reversed")] ;;; line 147
          [("G2 U相绕组温度超高故障") (cons 'reversed "Reversed")] ;;; line 148
          [("G2 V相绕组温度超高故障") (cons 'reversed "Reversed")] ;;; line 149
          [("G2 W相绕组温度超高故障") (cons 'reversed "Reversed")] ;;; line 150
          [("G2驱动端轴承温度超高故障") (cons 'reversed "Reversed")] ;;; line 151
          [("G2非驱动端轴承温度超高故障") (cons 'reversed "Reversed")] ;;; line 152
          [("G2功率模块温度超高故障") (cons 'reversed "Reversed")] ;;; line 153
          [("G2过载报警") (cons 'reversed "Reversed")] ;;; line 154
          [("G2电流较高报警") (cons 'reversed "Reversed")] ;;; line 155
          [("G2功率模块温度高报警") (cons 'reversed "Reversed")] ;;; line 156
          [("G2功率模块温度较高报警") (cons 'reversed "Reversed")] ;;; line 157
          [("G2柴油机二类综合故障") (cons 'reversed "Reversed")] ;;; line 158
          [("G2 U相绕组温度高报警") (cons 'reversed "Reversed")] ;;; line 159
          [("G2 V相绕组温度高报警") (cons 'reversed "Reversed")] ;;; line 160
          [("G2 W相绕组温度高报警") (cons 'reversed "Reversed")] ;;; line 161
          [("G2驱动端轴承温度高报警") (cons 'reversed "Reversed")] ;;; line 162
          [("G2非驱动端轴承温度高报警") (cons 'reversed "Reversed")] ;;; line 163
          [("G2 U相绕组温度较高报警") (cons 'reversed "Reversed")] ;;; line 164
          [("G2 V相绕组温度较高报警") (cons 'reversed "Reversed")] ;;; line 165
          [("G2 W相绕组温度较高报警") (cons 'reversed "Reversed")] ;;; line 166
          [("G2驱动端轴承温度较高报警") (cons 'reversed "Reversed")] ;;; line 167
          [("G2非驱动端轴承温度较高报警") (cons 'reversed "Reversed")] ;;; line 168
          [("G2其他综合报警") (cons 'reversed "Reversed")] ;;; line 169
          [("B1功率模块故障") (cons 'reversed "Reversed")] ;;; line 170
          [("B1瞬时过流故障") (cons 'reversed "Reversed")] ;;; line 171
          [("B1延时过流故障") (cons 'reversed "Reversed")] ;;; line 172
          [("B1主断路器CB2过流脱扣") (cons 'reversed "Reversed")] ;;; line 173
          [("B1直流熔断器损坏") (cons 'reversed "Reversed")] ;;; line 174
          [("B1功率模块温度超高故障") (cons 'reversed "Reversed")] ;;; line 175
          [("B1过载报警") (cons 'reversed "Reversed")] ;;; line 176
          [("B1电流较高报警") (cons 'reversed "Reversed")] ;;; line 177
          [("B1功率模块温度高报警") (cons 'reversed "Reversed")] ;;; line 178
          [("B1功率模块温度较高报警") (cons 'reversed "Reversed")] ;;; line 179
          [("B1其他综合报警") (cons 'reversed "Reversed")] ;;; line 180
          [("B1电抗器温度开关故障") (cons 'reversed "Reversed")] ;;; line 181
          [("T1功率模块故障") (cons 'reversed "Reversed")] ;;; line 185
          [("T1瞬时过流故障") (cons 'reversed "Reversed")] ;;; line 186
          [("T1延时过流故障") (cons 'reversed "Reversed")] ;;; line 187
          [("T1主断路器CB1过流脱扣") (cons 'reversed "Reversed")] ;;; line 188
          [("T1直流熔断器损坏") (cons 'reversed "Reversed")] ;;; line 189
          [("T1功率模块温度超高故障") (cons 'reversed "Reversed")] ;;; line 190
          [("T1过载报警") (cons 'reversed "Reversed")] ;;; line 191
          [("T1电流较高报警") (cons 'reversed "Reversed")] ;;; line 192
          [("T1功率模块温度高报警") (cons 'reversed "Reversed")] ;;; line 193
          [("T1功率模块温度较高报警") (cons 'reversed "Reversed")] ;;; line 194
          [("T1其他综合报警") (cons 'reversed "Reversed")] ;;; line 195
          [("T2功率模块故障") (cons 'reversed "Reversed")] ;;; line 196
          [("T2瞬时过流故障") (cons 'reversed "Reversed")] ;;; line 197
          [("T2延时过流故障") (cons 'reversed "Reversed")] ;;; line 198
          [("T2主断路器CB1过流脱扣") (cons 'reversed "Reversed")] ;;; line 199
          [("T2直流熔断器损坏") (cons 'reversed "Reversed")] ;;; line 200
          [("T2功率模块温度超高故障") (cons 'reversed "Reversed")] ;;; line 201
          [("T2过载报警") (cons 'reversed "Reversed")] ;;; line 202
          [("T2电流较高报警") (cons 'reversed "Reversed")] ;;; line 203
          [("T2功率模块温度高报警") (cons 'reversed "Reversed")] ;;; line 204
          [("T2功率模块温度较高报警") (cons 'reversed "Reversed")] ;;; line 205
          [("T2其他综合报警") (cons 'reversed "Reversed")] ;;; line 206
          [("H1其他综合报警") (cons 'reversed "Reversed")] ;;; line 217
          [("H1冷源水压力信号已断开") (cons 'reversed "Reversed")] ;;; line 218
          [("H1冷源水温度信号已断开") (cons 'reversed "Reversed")] ;;; line 219
          [("H1冷却水压力信号已断开") (cons 'reversed "Reversed")] ;;; line 220
          [("H1冷却水流量信号已断开") (cons 'reversed "Reversed")] ;;; line 221
          [("H1冷却水温度信号已断开") (cons 'reversed "Reversed")] ;;; line 222
          [("H1三通阀信号已断开") (cons 'reversed "Reversed")] ;;; line 223
          [("H1冷却水流量超高报警") (cons 'reversed "Reversed")] ;;; line 224
          [("H1冷却水流量高报警") (cons 'reversed "Reversed")] ;;; line 225
          [("H1冷却水流量超低报警") (cons 'reversed "Reversed")] ;;; line 226
          [("H1冷却水流量低报警") (cons 'reversed "Reversed")] ;;; line 227
          [("H1冷源水压力超高报警") (cons 'reversed "Reversed")] ;;; line 228
          [("H1冷源水压力高报警") (cons 'reversed "Reversed")] ;;; line 229
          [("H1冷源水压力超低报警") (cons 'reversed "Reversed")] ;;; line 230
          [("H1冷源水压力低报警") (cons 'reversed "Reversed")] ;;; line 231
          [("H1冷却水压力超高报警") (cons 'reversed "Reversed")] ;;; line 232
          [("H1冷却水压力高报警") (cons 'reversed "Reversed")] ;;; line 233
          [("H1冷却水压力超低报警") (cons 'reversed "Reversed")] ;;; line 234
          [("H1冷却水压力低报警") (cons 'reversed "Reversed")] ;;; line 235
          [("H1冷源水温度超高报警") (cons 'reversed "Reversed")] ;;; line 236
          [("H1冷源水温度高报警") (cons 'reversed "Reversed")] ;;; line 237
          [("H1冷源水温度超低报警") (cons 'reversed "Reversed")] ;;; line 238
          [("H1冷源水温度低报警") (cons 'reversed "Reversed")] ;;; line 239
          [("H1冷却水温度超高报警") (cons 'reversed "Reversed")] ;;; line 240
          [("H1冷却水温度高报警") (cons 'reversed "Reversed")] ;;; line 241
          [("H1冷却水温度超低报警") (cons 'reversed "Reversed")] ;;; line 242
          [("H1冷却水温度低报警") (cons 'reversed "Reversed")] ;;; line 243
          [("H1三通阀位置给定与反馈不等报警") (cons 'reversed "Reversed")] ;;; line 244
          [("H1冷却水温度超低且有泵运行报警") (cons 'reversed "Reversed")] ;;; line 245
          [("H1 220V供电丢失") (cons 'reversed "Reversed")] ;;; line 246
          [("G3功率模块故障") (cons 'reversed "Reversed")] ;;; line 249
          [("G3瞬时过流故障") (cons 'reversed "Reversed")] ;;; line 250
          [("G3延时过流故障") (cons 'reversed "Reversed")] ;;; line 251
          [("G3直流熔断器损坏") (cons 'reversed "Reversed")] ;;; line 252
          [("D1直流熔断器损坏") (cons 'reversed "Reversed")] ;;; line 253
          [("G3功率模块温度超高故障") (cons 'reversed "Reversed")] ;;; line 254
          [("G3过载报警") (cons 'reversed "Reversed")] ;;; line 255
          [("G3电流较高报警") (cons 'reversed "Reversed")] ;;; line 256
          [("G3功率模块温度高报警") (cons 'reversed "Reversed")] ;;; line 257
          [("G3功率模块温度较高报警") (cons 'reversed "Reversed")] ;;; line 258
          [("变频柜漏液检测报警") (cons 'reversed "Reversed")] ;;; line 259
          [("C1看门狗报警来自G1") (cons 'reversed "Reversed")] ;;; line 260
          [("C1看门狗报警来自G2") (cons 'reversed "Reversed")] ;;; line 261
          [("C1看门狗报警来自M1") (cons 'reversed "Reversed")] ;;; line 262
          [("C1看门狗报警来自M2") (cons 'reversed "Reversed")] ;;; line 263
          [("C1看门狗报警来自T1") (cons 'reversed "Reversed")] ;;; line 264
          [("C1看门狗报警来自T2") (cons 'reversed "Reversed")] ;;; line 265
          [("C1看门狗报警来自B1") (cons 'reversed "Reversed")] ;;; line 266
          [("C1看门狗报警来自H1") (cons 'reversed "Reversed")] ;;; line 267
          [("C1其他综合报警") (cons 'reversed "Reversed")] ;;; line 268
          [("C1左舷直流母线电压读取断开") (cons 'reversed "Reversed")] ;;; line 269
          [("C1右舷直流母线电压读取断开") (cons 'reversed "Reversed")] ;;; line 270
          [("C1滤波电抗器温控开关故障") (cons 'reversed "Reversed")] ;;; line 271
          [("G1启动失败") (cons 'reversed "Reversed")] ;;; line 281
          [("G1主断路器CB1合闸失败") (cons 'reversed "Reversed")] ;;; line 282
          [("G1岸电启动失败") (cons 'reversed "Reversed")] ;;; line 283
          [("G2启动失败") (cons 'reversed "Reversed")] ;;; line 284
          [("G2主断路器CB1合闸失败") (cons 'reversed "Reversed")] ;;; line 285
          [("M1启动失败") (cons 'reversed "Reversed")] ;;; line 286
          [("M2启动失败") (cons 'reversed "Reversed")] ;;; line 287
          [("T1启动失败") (cons 'reversed "Reversed")] ;;; line 288
          [("T2启动失败") (cons 'reversed "Reversed")] ;;; line 289
          [("B1启动失败") (cons 'reversed "Reversed")] ;;; line 290
          [("H1启动失败") (cons 'reversed "Reversed")] ;;; line 291
          [("G3启动失败") (cons 'reversed "Reversed")] ;;; line 292
          [("C1 220V配电板电源故障") (cons 'reversed "Reversed")] ;;; line 293
          [("C1 24V配电板电源故障") (cons 'reversed "Reversed")] ;;; line 294
          [("直流母线接地故障") (cons 'reversed "Reversed")] ;;; line 295
          [("PMS功率分配失败报警") (cons 'reversed "Reversed")] ;;; line 296
          [("当前状态保持,不做任何动作") (cons 'reversed "Reversed")] ;;; line 297
          [("当前处于停泊工况模式") (cons 'reversed "Reversed")] ;;; line 298
          [("当前处于离靠港工况模式") (cons 'reversed "Reversed")] ;;; line 299
          [("当前处于航行工况模式") (cons 'reversed "Reversed")] ;;; line 300
          [("当前处于长时间海岛系泊工况模式") (cons 'reversed "Reversed")] ;;; line 301
          [("当前处于顺水观光工况模式") (cons 'reversed "Reversed")] ;;; line 302
          [("当前处于短时间海岛系泊工况模式") (cons 'reversed "Reversed")] ;;; line 303
          [("控制电源电压低报警") (cons 'reversed "Reversed")] ;;; line 313
          [("冷却水温度高报警") (cons 'reversed "Reversed")] ;;; line 314
          [("滑油压力低报警") (cons 'reversed "Reversed")] ;;; line 315
          [("滑油压力高报警") (cons 'reversed "Reversed")] ;;; line 316
          [("超速报警") (cons 'reversed "Reversed")] ;;; line 317
          [("燃油泄露报警") (cons 'reversed "Reversed")] ;;; line 318
          [("海水压力低报警") (cons 'reversed "Reversed")] ;;; line 319
          [("燃油压力低报警") (cons 'reversed "Reversed")] ;;; line 320
          [("冷却水液位低报警") (cons 'reversed "Reversed")] ;;; line 321
          [("冷却水温度过高停车") (cons 'reversed "Reversed")] ;;; line 322
          [("滑油压力过低停车") (cons 'reversed "Reversed")] ;;; line 323
          [("超速停机") (cons 'reversed "Reversed")] ;;; line 324
          [("机旁急停") (cons 'reversed "Reversed")] ;;; line 325
          [("远程急停") (cons 'reversed "Reversed")] ;;; line 326
          [("公共报警") (cons 'reversed "Reversed")] ;;; line 327
          [("发动机运行状态") (cons 'reversed "Reversed")] ;;; line 328
          [("启动失败") (cons 'reversed "Reversed")] ;;; line 329
          [("滑油滤器压差高报警") (cons 'reversed "Reversed")] ;;; line 330
          [("备车完毕") (cons 'reversed "Reversed")] ;;; line 331
          [("1类故障报警") (cons 'reversed "Reversed")] ;;; line 332
          [("2类故障报警") (cons 'reversed "Reversed")] ;;; line 333
          [("冷却水压力低报警") (cons 'reversed "Reversed")] ;;; line 334
          [("允许合闸") (cons 'reversed "Reversed")] ;;; line 335
          [("控制电源电压低报警") (cons 'reversed "Reversed")] ;;; line 337
          [("冷却水温度高报警") (cons 'reversed "Reversed")] ;;; line 338
          [("滑油压力低报警") (cons 'reversed "Reversed")] ;;; line 339
          [("滑油压力高报警") (cons 'reversed "Reversed")] ;;; line 340
          [("超速报警") (cons 'reversed "Reversed")] ;;; line 341
          [("燃油泄露报警") (cons 'reversed "Reversed")] ;;; line 342
          [("海水压力低报警") (cons 'reversed "Reversed")] ;;; line 343
          [("燃油压力低报警") (cons 'reversed "Reversed")] ;;; line 344
          [("冷却水液位低报警") (cons 'reversed "Reversed")] ;;; line 345
          [("冷却水温度过高停车") (cons 'reversed "Reversed")] ;;; line 346
          [("滑油压力过低停车") (cons 'reversed "Reversed")] ;;; line 347
          [("超速停机") (cons 'reversed "Reversed")] ;;; line 348
          [("机旁急停") (cons 'reversed "Reversed")] ;;; line 349
          [("远程急停") (cons 'reversed "Reversed")] ;;; line 350
          [("公共报警") (cons 'reversed "Reversed")] ;;; line 351
          [("发动机运行状态") (cons 'reversed "Reversed")] ;;; line 352
          [("启动失败") (cons 'reversed "Reversed")] ;;; line 353
          [("滑油滤器压差高报警") (cons 'reversed "Reversed")] ;;; line 354
          [("备车完毕") (cons 'reversed "Reversed")] ;;; line 355
          [("1类故障报警") (cons 'reversed "Reversed")] ;;; line 356
          [("2类故障报警") (cons 'reversed "Reversed")] ;;; line 357
          [("冷却水压力低报警") (cons 'reversed "Reversed")] ;;; line 358
          [("允许合闸") (cons 'reversed "Reversed")] ;;; line 359
          [("驾控台手柄断线报警") (cons 'reversed "Reversed")] ;;; line 361
          [("直流母线系统通信故障") (cons 'reversed "Reversed")] ;;; line 362
          [("主电源故障报警") (cons 'reversed "Reversed")] ;;; line 363
          [("应急电源故障报警") (cons 'reversed "Reversed")] ;;; line 364
          [("随动控制失败报警") (cons 'reversed "Reversed")] ;;; line 365
          [("直流母线系统停机预报警") (cons 'reversed "Reversed")] ;;; line 366
          [("直流母线系统降速预报警") (cons 'reversed "Reversed")] ;;; line 367
          [("直流母线系统停机报警") (cons 'reversed "Reversed")] ;;; line 368
          [("直流母线系统降速报警") (cons 'reversed "Reversed")] ;;; line 369
          [("驾控台紧急停止") (cons 'reversed "Reversed")] ;;; line 370
          [("机旁紧急停止") (cons 'reversed "Reversed")] ;;; line 371
          [("直流母线系统紧急停止反馈") (cons 'reversed "Reversed")] ;;; line 372
          [("驾控台手柄断线报警") (cons 'reversed "Reversed")] ;;; line 373
          [("直流母线系统通信故障") (cons 'reversed "Reversed")] ;;; line 374
          [("主电源故障报警") (cons 'reversed "Reversed")] ;;; line 375
          [("应急电源故障报警") (cons 'reversed "Reversed")] ;;; line 376
          [("随动控制失败报警") (cons 'reversed "Reversed")] ;;; line 377
          [("直流母线系统停机预报警") (cons 'reversed "Reversed")] ;;; line 378
          [("直流母线系统降速预报警") (cons 'reversed "Reversed")] ;;; line 379
          [("直流母线系统停机报警") (cons 'reversed "Reversed")] ;;; line 380
          [("直流母线系统降速报警") (cons 'reversed "Reversed")] ;;; line 381
          [("驾控台紧急停止") (cons 'reversed "Reversed")] ;;; line 382
          [("机旁紧急停止") (cons 'reversed "Reversed")] ;;; line 383
          [("直流母线系统紧急停止反馈") (cons 'reversed "Reversed")] ;;; line 384
          [("M100(压缩机高压报警)") (cons 'reversed "Reversed")] ;;; line 385
          [("M101(压缩机低压报警)") (cons 'reversed "Reversed")] ;;; line 386
          [("M102(制热海水温度低,请开电加热报警)") (cons 'reversed "Reversed")] ;;; line 387
          [("M100(压缩机高压报警)") (cons 'reversed "Reversed")] ;;; line 388
          [("M101(压缩机低压报警)") (cons 'reversed "Reversed")] ;;; line 389
          [("M102(制热海水温度低,请开电加热报警)") (cons 'reversed "Reversed")] ;;; line 390
          [("M100(压缩机高压报警)") (cons 'reversed "Reversed")] ;;; line 391
          [("M101(压缩机低压报警)") (cons 'reversed "Reversed")] ;;; line 392
          [("M102(制热海水温度低,请开电加热报警)") (cons 'reversed "Reversed")] ;;; line 393
          [("M100(压缩机高压报警)") (cons 'reversed "Reversed")] ;;; line 394
          [("M101(压缩机低压报警)") (cons 'reversed "Reversed")] ;;; line 395
          [("M102(制热海水温度低,请开电加热报警)") (cons 'reversed "Reversed")] ;;; line 396
          [("M100(压缩机高压报警)") (cons 'reversed "Reversed")] ;;; line 397
          [("M101(压缩机低压报警)") (cons 'reversed "Reversed")] ;;; line 398
          [("M102(制热海水温度低,请开电加热报警)") (cons 'reversed "Reversed")] ;;; line 399
          [("驾驶室火灾报警") (cons 'reversed "Reversed")] ;;; line 401
          [("艏甲板火灾报警") (cons 'reversed "Reversed")] ;;; line 402
          [("艉甲板火灾报警") (cons 'reversed "Reversed")] ;;; line 403
          [("餐厅火灾报警") (cons 'reversed "Reversed")] ;;; line 404
          [("沙龙火灾报警") (cons 'reversed "Reversed")] ;;; line 405
          [("主人房间火灾报警") (cons 'reversed "Reversed")] ;;; line 406
          [("客房火灾报警") (cons 'reversed "Reversed")] ;;; line 407
          [("住舱通道火灾报警") (cons 'reversed "Reversed")] ;;; line 408
          [("梯道火灾报警") (cons 'reversed "Reversed")] ;;; line 409
          [("VIP房火灾报警") (cons 'reversed "Reversed")] ;;; line 410
          [("机舱火灾报警") (cons 'reversed "Reversed")] ;;; line 411
          [("艏尖舱火灾报警") (cons 'reversed "Reversed")] ;;; line 412
          [("蓄电池舱水位报警") (cons 'reversed "Reversed")] ;;; line 413
          [("艉尖舱水位报警") (cons 'reversed "Reversed")] ;;; line 414
          [("机舱水位报警") (cons 'reversed "Reversed")] ;;; line 415
          [("住舱水位报警") (cons 'reversed "Reversed")] ;;; line 416
          [("艏尖舱水位报警") (cons 'reversed "Reversed")] ;;; line 417
          [("制冷已开启") (cons 'reversed "Reversed")] ;;; line 433
          [("制冷已关闭") (cons 'reversed "Reversed")] ;;; line 434
          [("制热已开启") (cons 'reversed "Reversed")] ;;; line 435
          [("制热已关闭") (cons 'reversed "Reversed")] ;;; line 436
          [("电加热已开启") (cons 'reversed "Reversed")] ;;; line 437
          [("电加热已关闭") (cons 'reversed "Reversed")] ;;; line 438
          [("故障已复位") (cons 'reversed "Reversed")] ;;; line 439
          [("温度已设置") (cons 'reversed "Reversed")] ;;; line 440
          [("制冷已开启") (cons 'reversed "Reversed")] ;;; line 441
          [("制冷已关闭") (cons 'reversed "Reversed")] ;;; line 442
          [("制热已开启") (cons 'reversed "Reversed")] ;;; line 443
          [("制热已关闭") (cons 'reversed "Reversed")] ;;; line 444
          [("电加热已开启") (cons 'reversed "Reversed")] ;;; line 445
          [("电加热已关闭") (cons 'reversed "Reversed")] ;;; line 446
          [("故障已复位") (cons 'reversed "Reversed")] ;;; line 447
          [("温度已设置") (cons 'reversed "Reversed")] ;;; line 448
          [("制冷已开启") (cons 'reversed "Reversed")] ;;; line 449
          [("制冷已关闭") (cons 'reversed "Reversed")] ;;; line 450
          [("制热已开启") (cons 'reversed "Reversed")] ;;; line 451
          [("制热已关闭") (cons 'reversed "Reversed")] ;;; line 452
          [("电加热已开启") (cons 'reversed "Reversed")] ;;; line 453
          [("电加热已关闭") (cons 'reversed "Reversed")] ;;; line 454
          [("故障已复位") (cons 'reversed "Reversed")] ;;; line 455
          [("温度已设置") (cons 'reversed "Reversed")] ;;; line 456
          [("制冷已开启") (cons 'reversed "Reversed")] ;;; line 457
          [("制冷已关闭") (cons 'reversed "Reversed")] ;;; line 458
          [("制热已开启") (cons 'reversed "Reversed")] ;;; line 459
          [("制热已关闭") (cons 'reversed "Reversed")] ;;; line 460
          [("电加热已开启") (cons 'reversed "Reversed")] ;;; line 461
          [("电加热已关闭") (cons 'reversed "Reversed")] ;;; line 462
          [("故障已复位") (cons 'reversed "Reversed")] ;;; line 463
          [("温度已设置") (cons 'reversed "Reversed")] ;;; line 464
          [("PMS通信故障") (cons 'reversed "Reversed")] ;;; line 785
          [("1#发电机通信故障") (cons 'reversed "Reversed")] ;;; line 786
          [("2#发电机通信故障") (cons 'reversed "Reversed")] ;;; line 787
          [("左推进通信故障") (cons 'reversed "Reversed")] ;;; line 788
          [("右推进通信故障") (cons 'reversed "Reversed")] ;;; line 789
          [("空调通信故障") (cons 'reversed "Reversed")] ;;; line 790
          [("火灾通信故障") (cons 'reversed "Reversed")] ;;; line 791
          [("照明通信故障") (cons 'reversed "Reversed")] ;;; line 792
          [else #false]))

      (let-values ([(idx) (unbox &lineno)]
                   [(enum en_US) (if (pair? id) (values (car id) (cdr id)) (values 'Enum "en_US"))])
        (printf "[(~s) (cons '~s ~s)] ;;; line ~a~n" zh_CN enum en_US idx)
        (set-box! &lineno (add1 idx))
        (and (symbol? id) (aevent id idx en_US zh_CN))))))

(define DIs (filter-map (λ [row] (identify (list-ref row 4))) metrics))
