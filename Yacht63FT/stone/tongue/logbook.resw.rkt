#lang racket

(provide (all-defined-out))

(require syntax/location)

(require "../../../../Scripts/catalogue/csv.rkt")
(require "../../../../Scripts/catalogue/tongue.rkt")

(define ~level
  (lambda [level]
    (case level
      [(Over) "Exceed"]
      [(VeryHigh) "Very High"]
      [(Under) "Beneath"]
      [(UnderLoad) "Beneath and Loading"]
      [else level])))

(define ~dimension
  (lambda [d id?]
    (case d
      [(T) 'Temperature]
      [(V) 'Voltage]
      [(C) 'Current]
      [(R) 'Resistance]
      [(ω) (if id? 'Speed '|Rotational Speed|)]
      [(P) 'Pressure]
      [(Q) (if id? 'Flow '|Volumetric Flow Rate|)]
      [else d])))

(define ~name
  (lambda [name]
    (case name
      [(1) "#1"]
      [(2) "#2"]
      [(PS) "Port Station"]
      [(SB) "Starboard"]
      [(PM) "Power Module"]
      ((Upw) "U Phase Winding")
      ((Vpw) "V Phase Winding")
      ((Wpw) "W Phase Winding")
      [(DB) "Driven Bearing"]
      [(nDB) "non-Driven Bearing"]
      [(TB) "Thrust Bearing"]
      [(IOC) "Instantaneous Over Current Relay"]
      [(DTOC) "Definite Time Over Current Relay"]
      [(Bus) "DC Bus"]
      [(SPTransformer) "Shore Power Transformer"]
      [(SPCB) "Shore Power Circuit Breaker"]
      [(ACCoolant) "Air Conditioner Cooling Pump"]
      [(FWCoolant) "Fresh Water Cooling Pump"]
      [(SWCoolant) "Sea Water Cooling Pump"]
      [(Coolant) "Cooling Water"]
      [(Coolsrc) "Cooling Source Water"]
      [(BP) "Breaker Panel"]
      [(3WRValve) "3-Way Regulating Valve"]
      [else name])))

(define ~event
  (lambda [event]
    (case event
      [(Misc) "Miscellaneous Alerting"]
      [(1CMisc) "1st Class Miscellaneous Alerting"]
      [(2CMisc) "2nd Class Miscellaneous Alerting"]
      [(PowerFailure) "Power Failure"]
      [(TSwitchFault) "Temperature Switch Fault"]
      [(Mismatch) "Actual State Mismatches the Feedback"]
      [else event])))

(define dimension-fault
  (case-lambda
    [(D device part level)
     (cons (string->symbol (format "~a~a~a~a" device part level (~dimension D #true)))
           (format "~a ~a ~a ~a" (~name device) (~name part) (~dimension D #false) (~level level)))]
    [(D device level)
     (cons (string->symbol (format "~a~a~a" device level (~dimension D #true)))
           (format "~a ~a ~a" (~name device) (~dimension D #false) (~level level)))]))

(define status-event
  (case-lambda
    [(device part event)
     (cons (string->symbol (format "~a~a~a" device part event))
           (format "~a ~a ~a" (~name device) (~name part) (~event event)))]
    [(device event)
     (cons (string->symbol (format "~a~a" device event))
           (format "~a ~a" (~name device) (~event event)))]))

(define power-module-fault
  (case-lambda
    [(device)
     (cons (string->symbol (format "~aPMFault" device))
           (format "~a ~a Fault" device (~name 'PM)))]
    [(device temperature-level)
     (dimension-fault 'T device 'PM temperature-level)]))

(define protective-relay-fault
  (lambda [device type]
    (cons (string->symbol (format "~a~aFault" device type))
          (format "~a ~a Fault" device (~name type)))))

(define circuit-breaker-fault
  (lambda [device id event]
    (case event
      [(stripoff) (cons (string->symbol (format "~aCB~aTripOff" device id))
                        (format "~a CB~a Over Current Protective Relay Tripped Off" device id))]
      [(failure) (cons (string->symbol (format "~aCB~aSwitchingFailure" device id))
                       (format "~a CB~a Switching On Failure" device id))]
      [else (cons (string->symbol (format "~a~a" device id))
                  (format "~a" event))])))

(define fuse-broken
  (lambda [device]
    (cons (string->symbol (format "~aDCFuseBroken" device))
          (format "~a DC Fuse Broken" device))))

(define signal-lost-event
  (case-lambda
    [(device part D)
     (cons (string->symbol (format "~a~a~aLost" device part (~dimension D #true)))
           (format "~a ~a ~a signal lost" (~name device) (~name part) (~dimension D #false)))]
    [(device part)
     (cons (string->symbol (format "~a~aLost" device part))
           (format "~a ~a signal lost" (~name device) (~name part)))]))

(define identify
  (let ([&lineno (box 1)])
    (lambda [zh_CN]
      (define e
        (case zh_CN
          [("1#冷却淡水泵运行") (status-event 'FWCoolant 1 'Running)] ;;; line 1
          [("1#冷却淡水泵故障") (status-event 'FWCoolant 1 'Fault)]  ;;; line 2
          [("2#冷却淡水泵运行") (status-event 'FWCoolant 2 'Running)] ;;; line 3
          [("2#冷却淡水泵故障") (status-event 'FWCoolant 2 'Fault)] ;;; line 4
          [("1#冷却海水泵运行") (status-event 'SWCoolant 1 'Running)] ;;; line 5
          [("1#冷却海水泵故障") (status-event 'SWCoolant 1 'Fault)] ;;; line 6
          [("2#冷却海水泵运行") (status-event 'SWCoolant 2 'Running)] ;;; line 7
          [("2#冷却海水泵故障") (status-event 'SWCoolant 2 'Fault)] ;;; line 8
          [("DC24V配电板绝缘低") (dimension-fault 'R 'DC24V 'BP 'Low)] ;;; line 9
          [("AC220V配电板绝缘低") (dimension-fault 'R 'AC220V 'BP 'Low)] ;;; line 10
          [("供水装置运行") (status-event 'WaterWorks 'Running)] ;;; line 11
          [("供水装置故障") (status-event 'WaterWorks 'Fault)] ;;; line 12
          [("空调冷却水泵运行") (status-event 'ACCoolant 'Running)] ;;; line 17
          [("空调冷却水泵故障") (status-event 'ACCoolant 'Fault)] ;;; line 18
          [("CO2失电报警") (status-event 'CO2 'PowerFailure)] ;;; line 19
          [("CO2泄露报警") (status-event 'CO2 'Leak)] ;;; line 20
          [("CO2释放报警") (status-event 'CO2 'Release)] ;;; line 21

          [("M1功率模块故障") (power-module-fault 'M1)] ;;; line 25
          [("左舷直流电压超高故障") (dimension-fault 'V 'PS 'Bus 'Over)] ;;; line 26
          [("左舷直流电压超低故障") (dimension-fault 'V 'PS 'Bus 'Under)] ;;; line 27
          [("M1瞬时过流故障") (protective-relay-fault 'M1 'IOC)] ;;; line 28
          [("M1延时过流故障") (protective-relay-fault 'M1 'DTOC)] ;;; line 29
          [("M1超速故障") (dimension-fault 'ω 'M1 'Motor 'Over)] ;;; line 30
          [("M1电机转速低故障") (dimension-fault 'ω 'M1 'Motor 'Under)] ;;; line 31
          [("M1主断路器CB1过流脱扣") (circuit-breaker-fault 'M1 1 'stripoff)] ;;; line 32
          [("M1直流熔断器损坏") (fuse-broken 'M1)] ;;; line 33
          [("M1 U相绕组温度超高故障") (dimension-fault 'T 'M1 'Upw 'Over)] ;;; line 34
          [("M1 V相绕组温度超高故障") (dimension-fault 'T 'M1 'Vpw 'Over)] ;;; line 35
          [("M1 W相绕组温度超高故障") (dimension-fault 'T 'M1 'Wpw 'Over)] ;;; line 36
          [("M1驱动端轴承温度超高故障") (dimension-fault 'T 'M1 'DB 'Over)] ;;; line 37
          [("M1非驱动端轴承温度超高故障") (dimension-fault 'T 'M1 'nDB 'Over)] ;;; line 38
          [("M1艉轴温度超高故障") (dimension-fault 'T 'M1 'TB 'Over)] ;;; line 39
          [("M1功率模块温度超高故障") (power-module-fault 'M1 'Over)] ;;; line 40
          [("左舷直流电压高报警") (dimension-fault 'V 'PS 'Bus 'High)] ;;; line 41
          [("左舷直流电压低报警") (dimension-fault 'V 'PS 'Bus 'Low)] ;;; line 42
          [("M1过载报警") (status-event 'M1 'Overload)] ;;; line 43
          [("M1电流较高报警") (dimension-fault 'C 'M1 'VeryHigh)] ;;; line 44
          [("M1功率模块温度高报警") (power-module-fault 'M1 'High)] ;;; line 45
          [("M1功率模块温度较高报警") (power-module-fault 'M1 'VeryHigh)] ;;; line 46
          [("M1电机处于锁轴状态") (status-event 'M1 'Motor 'Locked)] ;;; line 47
          [("M1 U相绕组温度高报警") (dimension-fault 'T 'M1 'Upw 'High)] ;;; line 48
          [("M1 V相绕组温度高报警") (dimension-fault 'T 'M1 'Vpw 'High)] ;;; line 49
          [("M1 W相绕组温度高报警") (dimension-fault 'T 'M1 'Wpw 'High)] ;;; line 50
          [("M1驱动端轴承温度高报警") (dimension-fault 'T 'M1 'DB 'High)] ;;; line 51
          [("M1非驱动端轴承温度高报警") (dimension-fault 'T 'M1 'nDB 'High)] ;;; line 52
          [("M1艉轴温度高报警") (dimension-fault 'T 'M1 'TB 'High)] ;;; line 53
          [("M1 U相绕组温度较高报警") (dimension-fault 'T 'M1 'Upw 'VeryHigh)] ;;; line 54
          [("M1 V相绕组温度较高报警") (dimension-fault 'T 'M1 'Vpw 'VeryHigh)] ;;; line 55
          [("M1 W相绕组温度较高报警") (dimension-fault 'T 'M1 'Wpw 'VeryHigh)] ;;; line 56
          [("M1驱动端轴承温度较高报警") (dimension-fault 'T 'M1 'DB 'VeryHigh)] ;;; line 57
          [("M1非驱动端轴承温度较高报警") (dimension-fault 'T 'M1 'nDB 'VeryHigh)] ;;; line 58
          [("M1艉轴温度较高报警") (dimension-fault 'T 'M1 'TB 'VeryHigh)] ;;; line 59
          [("M1其他综合报警") (status-event 'M1 'Misc)] ;;; line 60

          [("M2功率模块故障") (power-module-fault 'M2)] ;;; line 61
          [("右舷直流电压超高故障") (dimension-fault 'V 'SB 'Bus 'Over)] ;;; line 62
          [("右舷直流电压超低故障") (dimension-fault 'V 'SB 'Bus 'Under)] ;;; line 63
          [("M2瞬时过流故障") (protective-relay-fault 'M2 'IOC)] ;;; line 64
          [("M2延时过流故障") (protective-relay-fault 'M2 'DTOC)] ;;; line 65
          [("M2超速故障") (dimension-fault 'ω 'M2 'Motor 'Over)] ;;; line 66
          [("M2电机转速低故障") (dimension-fault 'ω 'M2 'Motor 'Under)] ;;; line 67
          [("M2主断路器CB1过流脱扣") (circuit-breaker-fault 'M2 1 'stripoff)] ;;; line 68
          [("M2直流熔断器损坏") (fuse-broken 'M2)] ;;; line 69
          [("M2 U相绕组温度超高故障") (dimension-fault 'T 'M2 'U 'Over)] ;;; line 70
          [("M2 V相绕组温度超高故障") (dimension-fault 'T 'M2 'V 'Over)] ;;; line 71
          [("M2 W相绕组温度超高故障") (dimension-fault 'T 'M2 'W 'Over)] ;;; line 72
          [("M2驱动端轴承温度超高故障") (dimension-fault 'T 'M2 'DB 'Over)] ;;; line 73
          [("M2非驱动端轴承温度超高故障") (dimension-fault 'T 'M2 'nDB 'Over)] ;;; line 74
          [("M2艉轴温度超高故障") (dimension-fault 'T 'M2 'TB 'Over)] ;;; line 75
          [("M2功率模块温度超高故障") (power-module-fault 'M2 'Over)] ;;; line 76
          [("右舷直流电压高报警") (dimension-fault 'V 'SB 'Bus 'High)] ;;; line 77
          [("右舷直流电压低报警") (dimension-fault 'V 'SB 'Bus 'Low)] ;;; line 78
          [("M2过载报警") (status-event 'M2 'Overload)] ;;; line 79
          [("M2电流较高报警") (dimension-fault 'C 'M2 'VeryHigh)] ;;; line 80
          [("M2功率模块温度高报警") (power-module-fault 'M2 'High)] ;;; line 81
          [("M2功率模块温度较高报警") (power-module-fault 'M2 'VeryHigh)] ;;; line 82
          [("M2电机处于锁轴状态") (status-event 'M2 'Motor 'Locked)] ;;; line 83
          [("M2 U相绕组温度高报警") (dimension-fault 'T 'M2 'Upw 'High)] ;;; line 84
          [("M2 V相绕组温度高报警") (dimension-fault 'T 'M2 'Vpw 'High)] ;;; line 85
          [("M2 W相绕组温度高报警") (dimension-fault 'T 'M2 'Wpw 'High)] ;;; line 86
          [("M2驱动端轴承温度高报警") (dimension-fault 'T 'M2 'DB 'High)] ;;; line 87
          [("M2非驱动端轴承温度高报警") (dimension-fault 'T 'M2 'nDB 'High)] ;;; line 88
          [("M2艉轴温度高报警") (dimension-fault 'T 'M2 'TB 'High)] ;;; line 89
          [("M2 U相绕组温度较高报警") (dimension-fault 'T 'M2 'Upw 'VeryHigh)] ;;; line 90
          [("M2 V相绕组温度较高报警") (dimension-fault 'T 'M2 'Vpw 'VeryHigh)] ;;; line 91
          [("M2 W相绕组温度较高报警") (dimension-fault 'T 'M2 'Wpw 'VeryHigh)] ;;; line 92
          [("M2驱动端轴承温度较高报警") (dimension-fault 'T 'M2 'DB 'VeryHigh)] ;;; line 93
          [("M2非驱动端轴承温度较高报警") (dimension-fault 'T 'M2 'nDB 'VeryHigh)] ;;; line 94
          [("M2艉轴温度较高报警") (dimension-fault 'T 'M2 'TB 'VeryHigh)] ;;; line 95
          [("M2其他综合报警") (status-event 'M2 'Misc)] ;;; line 96

          [("G1功率模块故障") (power-module-fault 'G1)] ;;; line 105
          [("G1瞬时过流故障") (protective-relay-fault 'G1 'IOC)] ;;; line 106
          [("G1延时过流故障") (protective-relay-fault 'G1 'DTOC)] ;;; line 107
          [("G1超速故障") (dimension-fault 'ω 'G1 'Motor 'Over)] ;;; line 108
          [("G1电机转速低故障") (dimension-fault 'ω 'G1 'Motor 'Under)] ;;; line 109
          [("G1柴油机一类综合故障") (status-event 'G1 'Diesel '1CMisc)] ;;; line 110
          [("G1主断路器CB1过流脱扣") (circuit-breaker-fault 'G1 1 'stripoff)] ;;; line 111
          [("G1直流熔断器损坏") (fuse-broken 'G1)] ;;; line 112
          [("G1 U相绕组温度超高故障") (dimension-fault 'T 'G1 'Upw 'Over)] ;;; line 113
          [("G1 V相绕组温度超高故障") (dimension-fault 'T 'G1 'Vpw 'Over)] ;;; line 114
          [("G1 W相绕组温度超高故障") (dimension-fault 'T 'G1 'Wpw 'Over)] ;;; line 115
          [("G1驱动端轴承温度超高故障") (dimension-fault 'T 'G1 'DB 'Over)] ;;; line 116
          [("G1非驱动端轴承温度超高故障") (dimension-fault 'T 'G1 'nDB 'Over)] ;;; line 117
          [("G1岸电电源变压器温度超高故障") (dimension-fault 'T 'G1 'SPTransformer 'Over)] ;;; line 118
          [("G1功率模块温度超高故障") (power-module-fault 'G1 'Over)] ;;; line 119
          [("G1过载报警") (status-event 'G1 'Overload)] ;;; line 120
          [("G1电流较高报警") (dimension-fault 'C 'G1 'VeryHigh)] ;;; line 121
          [("G1功率模块温度高报警") (power-module-fault 'G1 'High)] ;;; line 122
          [("G1功率模块温度较高报警") (power-module-fault 'G1 'VeryHigh)] ;;; line 123
          [("G1柴油机二类综合故障") (status-event 'G1 'Diesel '2CMisc)] ;;; line 124
          [("G1 U相绕组温度高报警") (dimension-fault 'T 'G1 'Upw 'High)] ;;; line 125
          [("G1 V相绕组温度高报警") (dimension-fault 'T 'G1 'Vpw 'High)] ;;; line 126
          [("G1 W相绕组温度高报警") (dimension-fault 'T 'G1 'Wpw 'High)] ;;; line 127
          [("G1驱动端轴承温度高报警") (dimension-fault 'T 'G1 'DB 'High)] ;;; line 128
          [("G1非驱动端轴承温度高报警") (dimension-fault 'T 'G1 'nDB 'High)] ;;; line 129
          [("G1岸电电源变压器温度高报警") (dimension-fault 'T 'G1 'SPTransformer 'High)] ;;; line 130
          [("G1 U相绕组温度较高报警") (dimension-fault 'T 'G1 'Upw 'VeryHigh)] ;;; line 131
          [("G1 V相绕组温度较高报警") (dimension-fault 'T 'G1 'Vpw 'VeryHigh)] ;;; line 132
          [("G1 W相绕组温度较高报警") (dimension-fault 'T 'G1 'Wpw 'VeryHigh)] ;;; line 133
          [("G1驱动端轴承温度较高报警") (dimension-fault 'T 'G1 'DB 'VeryHigh)] ;;; line 134
          [("G1非驱动端轴承温度较高报警") (dimension-fault 'T 'G1 'nDB 'VeryHigh)] ;;; line 135
          [("G1岸电电源变压器温度较高报警") (dimension-fault 'T 'G1 'SPTransformer 'VeryHigh)] ;;; line 136
          [("G1岸电主断路器断开") (status-event 'G1 'SPCB 'OFF)] ;;; line 137
          [("G1其他综合报警") (status-event 'G1 'Misc)] ;;; line 138
          [("G1岸电变压器温度开关故障") (status-event 'G1 'SPTransformer 'TSwitchFault)] ;;; line 139

          [("G2功率模块故障") (power-module-fault 'G2)] ;;; line 140
          [("G2瞬时过流故障") (protective-relay-fault 'G2 'IOC)] ;;; line 141
          [("G2延时过流故障") (protective-relay-fault 'G2 'DTOC)] ;;; line 142
          [("G2超速故障") (dimension-fault 'ω 'G2 'Motor 'Over)] ;;; line 143
          [("G2电机转速低故障") (dimension-fault 'ω 'G2 'Motor 'Under)] ;;; line 144
          [("G2柴油机一类综合故障") (status-event 'G2 'Diesel '1CMisc)] ;;; line 145
          [("G2主断路器CB1过流脱扣") (circuit-breaker-fault 'G2 1 'stripoff)] ;;; line 146
          [("G2直流熔断器损坏") (fuse-broken 'G2)] ;;; line 147
          [("G2 U相绕组温度超高故障") (dimension-fault 'T 'G2 'Upw 'Over)] ;;; line 148
          [("G2 V相绕组温度超高故障") (dimension-fault 'T 'G2 'Vpw 'Over)] ;;; line 149
          [("G2 W相绕组温度超高故障") (dimension-fault 'T 'G2 'Wpw 'Over)] ;;; line 150
          [("G2驱动端轴承温度超高故障") (dimension-fault 'T 'G2 'DB 'Over)] ;;; line 151
          [("G2非驱动端轴承温度超高故障") (dimension-fault 'T 'G2 'nDB 'Over)] ;;; line 152
          [("G2功率模块温度超高故障") (power-module-fault 'G2 'Over)] ;;; line 153
          [("G2过载报警") (status-event 'G2 'Overload)] ;;; line 154
          [("G2电流较高报警") (dimension-fault 'C 'G2 'VeryHigh)] ;;; line 155
          [("G2功率模块温度高报警") (power-module-fault 'G2 'High)] ;;; line 156
          [("G2功率模块温度较高报警") (power-module-fault 'G2 'VeryHigh)] ;;; line 157
          [("G2柴油机二类综合故障") (status-event 'G2 'Diesel '2CMisc)] ;;; line 158
          [("G2 U相绕组温度高报警") (dimension-fault 'T 'G2 'Upw 'High)] ;;; line 159
          [("G2 V相绕组温度高报警") (dimension-fault 'T 'G2 'Vpw 'High)] ;;; line 160
          [("G2 W相绕组温度高报警") (dimension-fault 'T 'G2 'Wpw 'High)] ;;; line 161
          [("G2驱动端轴承温度高报警") (dimension-fault 'T 'G2 'DB 'High)] ;;; line 162
          [("G2非驱动端轴承温度高报警") (dimension-fault 'T 'G2 'nDB 'High)] ;;; line 163
          [("G2 U相绕组温度较高报警") (dimension-fault 'T 'G2 'Upw 'VeryHigh)] ;;; line 164
          [("G2 V相绕组温度较高报警") (dimension-fault 'T 'G2 'Vpw 'VeryHigh)] ;;; line 165
          [("G2 W相绕组温度较高报警") (dimension-fault 'T 'G2 'Wpw 'VeryHigh)] ;;; line 166
          [("G2驱动端轴承温度较高报警") (dimension-fault 'T 'G2 'DB 'VeryHigh)] ;;; line 167
          [("G2非驱动端轴承温度较高报警") (dimension-fault 'T 'G2 'nDB 'VeryHigh)] ;;; line 168
          [("G2其他综合报警") (status-event 'G2 'Misc)] ;;; line 169

          [("B1功率模块故障") (power-module-fault 'B1)] ;;; line 170
          [("B1瞬时过流故障") (protective-relay-fault 'B1 'IOC)] ;;; line 171
          [("B1延时过流故障") (protective-relay-fault 'B1 'DTOC)] ;;; line 172
          [("B1主断路器CB2过流脱扣") (circuit-breaker-fault 'B1 2 'stripoff)] ;;; line 173
          [("B1直流熔断器损坏") (fuse-broken 'B1)] ;;; line 174
          [("B1功率模块温度超高故障") (power-module-fault 'B1 'Over)] ;;; line 175
          [("B1过载报警") (status-event 'B1 'Overload)] ;;; line 176
          [("B1电流较高报警") (dimension-fault 'C 'B1 'VeryHigh)] ;;; line 177
          [("B1功率模块温度高报警") (power-module-fault 'B1 'High)] ;;; line 178
          [("B1功率模块温度较高报警") (power-module-fault 'B1 'VeryHigh)] ;;; line 179
          [("B1其他综合报警") (status-event 'B1 'Misc)] ;;; line 180
          [("B1电抗器温度开关故障") (status-event 'B1 'Inductor 'TSwitchFault)] ;;; line 181

          [("T1功率模块故障") (power-module-fault 'T1)] ;;; line 185
          [("T1瞬时过流故障") (protective-relay-fault 'T1 'IOC)] ;;; line 186
          [("T1延时过流故障") (protective-relay-fault 'T1 'DTOC)] ;;; line 187
          [("T1主断路器CB1过流脱扣") (circuit-breaker-fault 'T1 1 'stripoff)] ;;; line 188
          [("T1直流熔断器损坏") (fuse-broken 'T1)] ;;; line 189
          [("T1功率模块温度超高故障") (power-module-fault 'T1 'Over)] ;;; line 190
          [("T1过载报警") (status-event 'T1 'Overload)] ;;; line 191
          [("T1电流较高报警") (dimension-fault 'C 'T1 'VeryHigh)] ;;; line 192
          [("T1功率模块温度高报警") (power-module-fault 'T1 'High)] ;;; line 193
          [("T1功率模块温度较高报警") (power-module-fault 'T1 'VeryHigh)] ;;; line 194
          [("T1其他综合报警") (status-event 'T1 'Misc)] ;;; line 195

          [("T2功率模块故障") (power-module-fault 'T2)] ;;; line 196
          [("T2瞬时过流故障") (protective-relay-fault 'T2 'IOC)] ;;; line 197
          [("T2延时过流故障") (protective-relay-fault 'T2 'DTOC)] ;;; line 198
          [("T2主断路器CB1过流脱扣") (circuit-breaker-fault 'T2 1 'stripoff)] ;;; line 199
          [("T2直流熔断器损坏") (fuse-broken 'T2)] ;;; line 200
          [("T2功率模块温度超高故障") (power-module-fault 'T2 'Over)] ;;; line 201
          [("T2过载报警") (status-event 'T2 'Overload)] ;;; line 202
          [("T2电流较高报警") (dimension-fault 'C 'T2 'VeryHigh)] ;;; line 203
          [("T2功率模块温度高报警") (power-module-fault 'T2 'High)] ;;; line 204
          [("T2功率模块温度较高报警") (power-module-fault 'T2 'VeryHigh)] ;;; line 205
          [("T2其他综合报警") (status-event 'T2 'Misc)] ;;; line 206

          [("H1其他综合报警") (status-event 'H1 'Misc)] ;;; line 217
          [("H1冷源水压力信号已断开") (signal-lost-event 'H1 'Coolsrc 'P)] ;;; line 218
          [("H1冷源水温度信号已断开") (signal-lost-event 'H1 'Coolsrc 'T)] ;;; line 219
          [("H1冷却水压力信号已断开") (signal-lost-event 'H1 'Coolant 'P)] ;;; line 220
          [("H1冷却水流量信号已断开") (signal-lost-event 'H1 'Coolant 'Q)] ;;; line 221
          [("H1冷却水温度信号已断开") (signal-lost-event 'H1 'Coolant 'T)] ;;; line 222
          [("H1三通阀信号已断开") (signal-lost-event 'H1 '3WRValve)] ;;; line 223
          [("H1冷却水流量超高报警") (dimension-fault 'Q 'H1 'Coolant 'Over)] ;;; line 224
          [("H1冷却水流量高报警") (dimension-fault 'Q 'H1 'Coolant 'High)] ;;; line 225
          [("H1冷却水流量超低报警") (dimension-fault 'Q 'H1 'Coolant 'Under)] ;;; line 226
          [("H1冷却水流量低报警") (dimension-fault 'Q 'H1 'Coolant 'Low)] ;;; line 227
          [("H1冷源水压力超高报警") (dimension-fault 'P 'H1 'Coolsrc 'Over)] ;;; line 228
          [("H1冷源水压力高报警") (dimension-fault 'P 'H1 'Coolsrc 'High)] ;;; line 229
          [("H1冷源水压力超低报警") (dimension-fault 'P 'H1 'Coolsrc 'Under)] ;;; line 230
          [("H1冷源水压力低报警") (dimension-fault 'P 'H1 'Coolsrc 'Low)] ;;; line 231
          [("H1冷却水压力超高报警") (dimension-fault 'P 'H1 'Coolant 'Over)] ;;; line 232
          [("H1冷却水压力高报警") (dimension-fault 'P 'H1 'Coolant 'High)] ;;; line 233
          [("H1冷却水压力超低报警") (dimension-fault 'P 'H1 'Coolant 'Under)] ;;; line 234
          [("H1冷却水压力低报警") (dimension-fault 'P 'H1 'Coolant 'Low)] ;;; line 235
          [("H1冷源水温度超高报警") (dimension-fault 'T 'H1 'Coolsrc 'Over)] ;;; line 236
          [("H1冷源水温度高报警") (dimension-fault 'T 'H1 'Coolsrc 'High)] ;;; line 237
          [("H1冷源水温度超低报警") (dimension-fault 'T 'H1 'Coolsrc 'Under)] ;;; line 238
          [("H1冷源水温度低报警") (dimension-fault 'T 'H1 'Coolsrc 'Low)] ;;; line 239
          [("H1冷却水温度超高报警") (dimension-fault 'T 'H1 'Coolant 'Over)] ;;; line 240
          [("H1冷却水温度高报警") (dimension-fault 'T 'H1 'Coolant 'High)] ;;; line 241
          [("H1冷却水温度超低报警") (dimension-fault 'T 'H1 'Coolant 'Under)] ;;; line 242
          [("H1冷却水温度低报警") (dimension-fault 'T 'H1 'Coolant 'Low)] ;;; line 243
          [("H1三通阀位置给定与反馈不等报警") (status-event 'H1 '3WRValve 'Mismatch)] ;;; line 244
          [("H1冷却水温度超低且有泵运行报警") (dimension-fault 'T 'H1 'Coolant 'UnderLoad)] ;;; line 245
          [("H1 220V供电丢失") (status-event 'H1 '220V 'PowerFailure)] ;;; line 246

          [("G3功率模块故障") (power-module-fault 'G3)] ;;; line 249
          [("G3瞬时过流故障") (protective-relay-fault 'G3 'IOC)] ;;; line 250
          [("G3延时过流故障") (protective-relay-fault 'G3 'DTOC)] ;;; line 251
          [("G3直流熔断器损坏") (fuse-broken 'G3)] ;;; line 252
          [("D1直流熔断器损坏") (fuse-broken 'D1)] ;;; line 253
          [("G3功率模块温度超高故障") (power-module-fault 'G3 'Over)] ;;; line 254
          [("G3过载报警") (status-event 'G3 'Overload)] ;;; line 255
          [("G3电流较高报警") (dimension-fault 'C 'G3 'VeryHigh)] ;;; line 256
          [("G3功率模块温度高报警") (power-module-fault 'G3 'High)] ;;; line 257
          [("G3功率模块温度较高报警") (power-module-fault 'G3 'VeryHigh)] ;;; line 258
          
          ;[("变频柜漏液检测报警") (cons 'reversed "Reversed")] ;;; line 259
          ;[("C1看门狗报警来自G1") (cons 'reversed "Reversed")] ;;; line 260
          ;[("C1看门狗报警来自G2") (cons 'reversed "Reversed")] ;;; line 261
          ;[("C1看门狗报警来自M1") (cons 'reversed "Reversed")] ;;; line 262
          ;[("C1看门狗报警来自M2") (cons 'reversed "Reversed")] ;;; line 263
          ;[("C1看门狗报警来自T1") (cons 'reversed "Reversed")] ;;; line 264
          ;[("C1看门狗报警来自T2") (cons 'reversed "Reversed")] ;;; line 265
          ;[("C1看门狗报警来自B1") (cons 'reversed "Reversed")] ;;; line 266
          ;[("C1看门狗报警来自H1") (cons 'reversed "Reversed")] ;;; line 267
          ;[("C1其他综合报警") (status-event 'C1 'Misc)] ;;; line 268
          ;[("C1左舷直流母线电压读取断开") (cons 'reversed "Reversed")] ;;; line 269
          ;[("C1右舷直流母线电压读取断开") (cons 'reversed "Reversed")] ;;; line 270
          ;[("C1滤波电抗器温控开关故障") (cons 'reversed "Reversed")] ;;; line 271
          ;[("G1启动失败") (cons 'reversed "Reversed")] ;;; line 281
          ;[("G1主断路器CB1合闸失败") (circuit-breaker-fault 'G1 1 'failure)] ;;; line 282
          ;[("G1岸电启动失败") (cons 'reversed "Reversed")] ;;; line 283
          ;[("G2启动失败") (cons 'reversed "Reversed")] ;;; line 284
          ;[("G2主断路器CB1合闸失败") (circuit-breaker-fault 'G1 1 'failure)] ;;; line 285
          ;[("M1启动失败") (cons 'reversed "Reversed")] ;;; line 286
          ;[("M2启动失败") (cons 'reversed "Reversed")] ;;; line 287
          ;[("T1启动失败") (cons 'reversed "Reversed")] ;;; line 288
          ;[("T2启动失败") (cons 'reversed "Reversed")] ;;; line 289
          ;[("B1启动失败") (cons 'reversed "Reversed")] ;;; line 290
          ;[("H1启动失败") (cons 'reversed "Reversed")] ;;; line 291
          ;[("G3启动失败") (cons 'reversed "Reversed")] ;;; line 292
          ;[("C1 220V配电板电源故障") (cons 'reversed "Reversed")] ;;; line 293
          ;[("C1 24V配电板电源故障") (cons 'reversed "Reversed")] ;;; line 294
          ;[("直流母线接地故障") (cons 'reversed "Reversed")] ;;; line 295
          ;[("PMS功率分配失败报警") (cons 'reversed "Reversed")] ;;; line 296
          ;[("当前状态保持,不做任何动作") (cons 'reversed "Reversed")] ;;; line 297
          ;[("当前处于停泊工况模式") (cons 'reversed "Reversed")] ;;; line 298
          ;[("当前处于离靠港工况模式") (cons 'reversed "Reversed")] ;;; line 299
          ;[("当前处于航行工况模式") (cons 'reversed "Reversed")] ;;; line 300
          ;[("当前处于长时间海岛系泊工况模式") (cons 'reversed "Reversed")] ;;; line 301
          ;[("当前处于顺水观光工况模式") (cons 'reversed "Reversed")] ;;; line 302
          ;[("当前处于短时间海岛系泊工况模式") (cons 'reversed "Reversed")] ;;; line 303
          ;[("控制电源电压低报警") (cons 'reversed "Reversed")] ;;; line 313
          ;[("冷却水温度高报警") (cons 'reversed "Reversed")] ;;; line 314
          ;[("滑油压力低报警") (cons 'reversed "Reversed")] ;;; line 315
          ;[("滑油压力高报警") (cons 'reversed "Reversed")] ;;; line 316
          ;[("超速报警") (cons 'reversed "Reversed")] ;;; line 317
          ;[("燃油泄露报警") (cons 'reversed "Reversed")] ;;; line 318
          ;[("海水压力低报警") (cons 'reversed "Reversed")] ;;; line 319
          ;[("燃油压力低报警") (cons 'reversed "Reversed")] ;;; line 320
          ;[("冷却水液位低报警") (cons 'reversed "Reversed")] ;;; line 321
          ;[("冷却水温度过高停车") (cons 'reversed "Reversed")] ;;; line 322
          ;[("滑油压力过低停车") (cons 'reversed "Reversed")] ;;; line 323
          ;[("超速停机") (cons 'reversed "Reversed")] ;;; line 324
          ;[("机旁急停") (cons 'reversed "Reversed")] ;;; line 325
          ;[("远程急停") (cons 'reversed "Reversed")] ;;; line 326
          ;[("公共报警") (cons 'reversed "Reversed")] ;;; line 327
          ;[("发动机运行状态") (cons 'reversed "Reversed")] ;;; line 328
          ;[("启动失败") (cons 'reversed "Reversed")] ;;; line 329
          ;[("滑油滤器压差高报警") (cons 'reversed "Reversed")] ;;; line 330
          ;[("备车完毕") (cons 'reversed "Reversed")] ;;; line 331
          ;[("1类故障报警") (cons 'reversed "Reversed")] ;;; line 332
          ;[("2类故障报警") (cons 'reversed "Reversed")] ;;; line 333
          ;[("冷却水压力低报警") (cons 'reversed "Reversed")] ;;; line 334
          ;[("允许合闸") (cons 'reversed "Reversed")] ;;; line 335
          ;[("控制电源电压低报警") (cons 'reversed "Reversed")] ;;; line 337
          ;[("冷却水温度高报警") (cons 'reversed "Reversed")] ;;; line 338
          ;[("滑油压力低报警") (cons 'reversed "Reversed")] ;;; line 339
          ;[("滑油压力高报警") (cons 'reversed "Reversed")] ;;; line 340
          ;[("超速报警") (cons 'reversed "Reversed")] ;;; line 341
          ;[("燃油泄露报警") (cons 'reversed "Reversed")] ;;; line 342
          ;[("海水压力低报警") (cons 'reversed "Reversed")] ;;; line 343
          ;[("燃油压力低报警") (cons 'reversed "Reversed")] ;;; line 344
          ;[("冷却水液位低报警") (cons 'reversed "Reversed")] ;;; line 345
          ;[("冷却水温度过高停车") (cons 'reversed "Reversed")] ;;; line 346
          ;[("滑油压力过低停车") (cons 'reversed "Reversed")] ;;; line 347
          ;[("超速停机") (cons 'reversed "Reversed")] ;;; line 348
          ;[("机旁急停") (cons 'reversed "Reversed")] ;;; line 349
          ;[("远程急停") (cons 'reversed "Reversed")] ;;; line 350
          ;[("公共报警") (cons 'reversed "Reversed")] ;;; line 351
          ;[("发动机运行状态") (cons 'reversed "Reversed")] ;;; line 352
          ;[("启动失败") (cons 'reversed "Reversed")] ;;; line 353
          ;[("滑油滤器压差高报警") (cons 'reversed "Reversed")] ;;; line 354
          ;[("备车完毕") (cons 'reversed "Reversed")] ;;; line 355
          ;[("1类故障报警") (cons 'reversed "Reversed")] ;;; line 356
          ;[("2类故障报警") (cons 'reversed "Reversed")] ;;; line 357
          ;[("冷却水压力低报警") (cons 'reversed "Reversed")] ;;; line 358
          ;[("允许合闸") (cons 'reversed "Reversed")] ;;; line 359
          ;[("驾控台手柄断线报警") (cons 'reversed "Reversed")] ;;; line 361
          ;[("直流母线系统通信故障") (cons 'reversed "Reversed")] ;;; line 362
          ;[("主电源故障报警") (cons 'reversed "Reversed")] ;;; line 363
          ;[("应急电源故障报警") (cons 'reversed "Reversed")] ;;; line 364
          ;[("随动控制失败报警") (cons 'reversed "Reversed")] ;;; line 365
          ;[("直流母线系统停机预报警") (cons 'reversed "Reversed")] ;;; line 366
          ;[("直流母线系统降速预报警") (cons 'reversed "Reversed")] ;;; line 367
          ;[("直流母线系统停机报警") (cons 'reversed "Reversed")] ;;; line 368
          ;[("直流母线系统降速报警") (cons 'reversed "Reversed")] ;;; line 369
          ;[("驾控台紧急停止") (cons 'reversed "Reversed")] ;;; line 370
          ;[("机旁紧急停止") (cons 'reversed "Reversed")] ;;; line 371
          ;[("直流母线系统紧急停止反馈") (cons 'reversed "Reversed")] ;;; line 372
          ;[("驾控台手柄断线报警") (cons 'reversed "Reversed")] ;;; line 373
          ;[("直流母线系统通信故障") (cons 'reversed "Reversed")] ;;; line 374
          ;[("主电源故障报警") (cons 'reversed "Reversed")] ;;; line 375
          ;[("应急电源故障报警") (cons 'reversed "Reversed")] ;;; line 376
          ;[("随动控制失败报警") (cons 'reversed "Reversed")] ;;; line 377
          ;[("直流母线系统停机预报警") (cons 'reversed "Reversed")] ;;; line 378
          ;[("直流母线系统降速预报警") (cons 'reversed "Reversed")] ;;; line 379
          ;[("直流母线系统停机报警") (cons 'reversed "Reversed")] ;;; line 380
          ;[("直流母线系统降速报警") (cons 'reversed "Reversed")] ;;; line 381
          ;[("驾控台紧急停止") (cons 'reversed "Reversed")] ;;; line 382
          ;[("机旁紧急停止") (cons 'reversed "Reversed")] ;;; line 383
          ;[("直流母线系统紧急停止反馈") (cons 'reversed "Reversed")] ;;; line 384
          ;[("M100(压缩机高压报警)") (cons 'reversed "Reversed")] ;;; line 385
          ;[("M101(压缩机低压报警)") (cons 'reversed "Reversed")] ;;; line 386
          ;[("M102(制热海水温度低,请开电加热报警)") (cons 'reversed "Reversed")] ;;; line 387
          ;[("M100(压缩机高压报警)") (cons 'reversed "Reversed")] ;;; line 388
          ;[("M101(压缩机低压报警)") (cons 'reversed "Reversed")] ;;; line 389
          ;[("M102(制热海水温度低,请开电加热报警)") (cons 'reversed "Reversed")] ;;; line 390
          ;[("M100(压缩机高压报警)") (cons 'reversed "Reversed")] ;;; line 391
          ;[("M101(压缩机低压报警)") (cons 'reversed "Reversed")] ;;; line 392
          ;[("M102(制热海水温度低,请开电加热报警)") (cons 'reversed "Reversed")] ;;; line 393
          ;[("M100(压缩机高压报警)") (cons 'reversed "Reversed")] ;;; line 394
          ;[("M101(压缩机低压报警)") (cons 'reversed "Reversed")] ;;; line 395
          ;[("M102(制热海水温度低,请开电加热报警)") (cons 'reversed "Reversed")] ;;; line 396
          ;[("M100(压缩机高压报警)") (cons 'reversed "Reversed")] ;;; line 397
          ;[("M101(压缩机低压报警)") (cons 'reversed "Reversed")] ;;; line 398
          ;[("M102(制热海水温度低,请开电加热报警)") (cons 'reversed "Reversed")] ;;; line 399
          ;[("驾驶室火灾报警") (cons 'reversed "Reversed")] ;;; line 401
          ;[("艏甲板火灾报警") (cons 'reversed "Reversed")] ;;; line 402
          ;[("艉甲板火灾报警") (cons 'reversed "Reversed")] ;;; line 403
          ;[("餐厅火灾报警") (cons 'reversed "Reversed")] ;;; line 404
          ;[("沙龙火灾报警") (cons 'reversed "Reversed")] ;;; line 405
          ;[("主人房间火灾报警") (cons 'reversed "Reversed")] ;;; line 406
          ;[("客房火灾报警") (cons 'reversed "Reversed")] ;;; line 407
          ;[("住舱通道火灾报警") (cons 'reversed "Reversed")] ;;; line 408
          ;[("梯道火灾报警") (cons 'reversed "Reversed")] ;;; line 409
          ;[("VIP房火灾报警") (cons 'reversed "Reversed")] ;;; line 410
          ;[("机舱火灾报警") (cons 'reversed "Reversed")] ;;; line 411
          ;[("艏尖舱火灾报警") (cons 'reversed "Reversed")] ;;; line 412
          ;[("蓄电池舱水位报警") (cons 'reversed "Reversed")] ;;; line 413
          ;[("艉尖舱水位报警") (cons 'reversed "Reversed")] ;;; line 414
          ;[("机舱水位报警") (cons 'reversed "Reversed")] ;;; line 415
          ;[("住舱水位报警") (cons 'reversed "Reversed")] ;;; line 416
          ;[("艏尖舱水位报警") (cons 'reversed "Reversed")] ;;; line 417
          ;[("制冷已开启") (cons 'reversed "Reversed")] ;;; line 433
          ;[("制冷已关闭") (cons 'reversed "Reversed")] ;;; line 434
          ;[("制热已开启") (cons 'reversed "Reversed")] ;;; line 435
          ;[("制热已关闭") (cons 'reversed "Reversed")] ;;; line 436
          ;[("电加热已开启") (cons 'reversed "Reversed")] ;;; line 437
          ;[("电加热已关闭") (cons 'reversed "Reversed")] ;;; line 438
          ;[("故障已复位") (cons 'reversed "Reversed")] ;;; line 439
          ;[("温度已设置") (cons 'reversed "Reversed")] ;;; line 440
          ;[("制冷已开启") (cons 'reversed "Reversed")] ;;; line 441
          ;[("制冷已关闭") (cons 'reversed "Reversed")] ;;; line 442
          ;[("制热已开启") (cons 'reversed "Reversed")] ;;; line 443
          ;[("制热已关闭") (cons 'reversed "Reversed")] ;;; line 444
          ;[("电加热已开启") (cons 'reversed "Reversed")] ;;; line 445
          ;[("电加热已关闭") (cons 'reversed "Reversed")] ;;; line 446
          ;[("故障已复位") (cons 'reversed "Reversed")] ;;; line 447
          ;[("温度已设置") (cons 'reversed "Reversed")] ;;; line 448
          ;[("制冷已开启") (cons 'reversed "Reversed")] ;;; line 449
          ;[("制冷已关闭") (cons 'reversed "Reversed")] ;;; line 450
          ;[("制热已开启") (cons 'reversed "Reversed")] ;;; line 451
          ;[("制热已关闭") (cons 'reversed "Reversed")] ;;; line 452
          ;[("电加热已开启") (cons 'reversed "Reversed")] ;;; line 453
          ;[("电加热已关闭") (cons 'reversed "Reversed")] ;;; line 454
          ;[("故障已复位") (cons 'reversed "Reversed")] ;;; line 455
          ;[("温度已设置") (cons 'reversed "Reversed")] ;;; line 456
          ;[("制冷已开启") (cons 'reversed "Reversed")] ;;; line 457
          ;[("制冷已关闭") (cons 'reversed "Reversed")] ;;; line 458
          ;[("制热已开启") (cons 'reversed "Reversed")] ;;; line 459
          ;[("制热已关闭") (cons 'reversed "Reversed")] ;;; line 460
          ;[("电加热已开启") (cons 'reversed "Reversed")] ;;; line 461
          ;[("电加热已关闭") (cons 'reversed "Reversed")] ;;; line 462
          ;[("故障已复位") (cons 'reversed "Reversed")] ;;; line 463
          ;[("温度已设置") (cons 'reversed "Reversed")] ;;; line 464
          ;[("PMS通信故障") (cons 'reversed "Reversed")] ;;; line 785
          ;[("1#发电机通信故障") (cons 'reversed "Reversed")] ;;; line 786
          ;[("2#发电机通信故障") (cons 'reversed "Reversed")] ;;; line 787
          ;[("左推进通信故障") (cons 'reversed "Reversed")] ;;; line 788
          ;[("右推进通信故障") (cons 'reversed "Reversed")] ;;; line 789
          ;[("空调通信故障") (cons 'reversed "Reversed")] ;;; line 790
          ;[("火灾通信故障") (cons 'reversed "Reversed")] ;;; line 791
          ;[("照明通信故障") (cons 'reversed "Reversed")] ;;; line 792
          [else #false]))

      (let-values ([(idx) (unbox &lineno)]
                   [(id en_US) (if (pair? e) (values (car e) (cdr e)) (values 'ID "en_US"))])
        (unless (pair? e)
          (printf "; [(~s) (cons '~s ~s)] ;;; line ~a~n" zh_CN id en_US idx))
        (set-box! &lineno (add1 idx))
        (and (pair? e) (tongue id idx en_US zh_CN))))))

(define main
  (lambda []
    (define src.csv (build-path (path-only (quote-source-file)) "catalogue.csv"))
    (define metrics (read-csv src.csv))
    (values 'Logbook
            (filter-map (λ [row] (identify (list-ref row 4))) metrics)
            1
            (length metrics))))

(module+ main (main))
