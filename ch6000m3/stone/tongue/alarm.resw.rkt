#lang racket

(provide (all-defined-out))

(require syntax/location)

(require "../../../Scripts/catalogue/tongue.rkt")

(define line-splite
  (lambda [line]
    (define tokens (regexp-match #px"DB(\\d+)[.]DBX(\\d+)[.]([0-7]);[^/]+//\\s*(.+)\\s*$" line))
    (and (pair? tokens)
         (list (string->number (cadr tokens))
               (string->number (caddr tokens))
               (string->number (cadddr tokens))
               (last tokens)))))

(define alarm-code
  (lambda [DB idx bidx]
    (define offset (if (= DB 4) 4 204))
    (+ (* (+ idx offset) 8) bidx)))

(define alarm-id
  (lambda [DB idx bidx]
    (+ (arithmetic-shift DB 16)
       (+ (* idx 8) bidx))))

(define identify
  (lambda [tokens]
    (match-define (list DB idx bidx zh_CN) tokens)
    (define e
      (case zh_CN
        [("左舷舱内泵/水下泵变频器维修模式") (cons 'PSHopperMaintenance "PS Hopper/Underwater Converter Mainenance")] ;;; 39[DB4.DBX0.7]
        [("左舷舱内泵封水泵变频器A故障状态") (cons 'PSGlandPumpABroken "PS Master Gland Pump Broken")] ;;; 43[DB4.DBX1.3]
        [("左舷舱内泵封水泵变频器B故障状态") (cons 'PSGlandPumpBBroken "PS Spare Gland Pump Broken")] ;;; 47[DB4.DBX1.7]
        [("左舷高压冲水泵变频器报警状态") (cons 'PSWaterPumpAlert "PS Water Pump Alert")] ;;; 51[DB4.DBX2.3]
        [("左舷高压冲水泵变频器故障状态") (cons 'PSWaterPumpBroken "PS Water Pump Broken")] ;;; 52[DB4.DBX2.4]
        [("左舷高压冲水泵变频器维修模式") (cons 'PSWaterPumpMaintenance "PS Water Pump Maintenance")] ;;; 54[DB4.DBX2.6]
        [("左舷高压冲水泵变频器应急停止反馈") (cons 'PSWaterPumpEmergence "PS Water Pump Emergence")] ;;; 55[DB4.DBX2.7]
        [("右舷舱内泵/水下泵变频器维修模式") (cons 'SBHopperMaintenance "SB Hopper/Underwater Converter Mainenance")] ;;; 63[DB4.DBX3.7]
        [("右舷舱内泵封水泵变频器A故障状态") (cons 'SBGlandPumpABroken "SB Master Gland Pump Broken")] ;;; 67[DB4.DBX4.3]
        [("右舷舱内泵封水泵变频器B故障状态") (cons 'SBGlandPumpBBroken "SB Spare Gland Pump Broken")] ;;; 71[DB4.DBX4.7]
        [("右舷高压冲水泵变频器报警状态") (cons 'SBWaterPumpAlert "SB Water Pump Alert")] ;;; 75[DB4.DBX5.3]
        [("右舷高压冲水泵变频器故障状态") (cons 'SBWaterPumpBroken "SB Water Pump Broken")] ;;; 76[DB4.DBX5.4]
        [("右舷高压冲水泵变频器应急停止反馈") (cons 'SBWaterPumpMaintenance "SB Water Pump Emergence")] ;;; 79[DB4.DBX5.7]
        [("左舷耙头液压泵A故障反馈") (cons 'PSDHPumpABroken "PS Drag Head Pump A Broken")] ;;; 82[DB4.DBX6.2]
        [("左舷耙中液压泵B故障反馈") (cons 'PSDIPumpBBroken "PS Drag Intermediate Pump B Broken")] ;;; 86[DB4.DBX6.6]
        [("左舷弯管液压泵C故障反馈") (cons 'PSDTPumpCBroken "PS Drag Trunnion Pump C Broken")] ;;; 90[DB4.DBX7.2]
        [("右舷耙唇液压泵I故障反馈") (cons 'SBDVPumpIBroken "SB Drag Visor Pump I Broken")] ;;; 94[DB4.DBX7.6]
        [("右舷耙头液压泵H故障反馈") (cons 'SBDHPumpHBroken "SB Drag Head Pump H Broken")] ;;; 98[DB4.DBX8.2]
        [("右舷耙中液压泵G故障反馈") (cons 'SBDIPumpGBroken "SB Drag Intermediate Pump G Broken")] ;;; 102[DB4.DBX8.6]
        [("右舷弯管液压泵F故障反馈") (cons 'SBDTPumpFBroken "SB Drag Trunnion Pump F Broken")] ;;; 106[DB4.DBX9.2]
        [("左舷耙唇液压泵J故障反馈") (cons 'SBDVPumpJBroken "SB Drag Visor Pump J Broken")] ;;; 110[DB4.DBX9.6]
        [("泥泵锁紧/蝶阀控制泵D故障反馈") (cons 'DLBVPumpDBroken "Door Locker/Butterfly Valve Pump D Broken")] ;;; 114[DB4.DBX10.2]
        [("泥泵锁紧/蝶阀控制泵E故障反馈") (cons 'DLBVPumpDBroken "Door Locker/Butterfly Valve Pump E Broken")] ;;; 118[DB4.DBX10.6]
        [("液压冷却泵K故障反馈") (cons 'CoolanPumpKBroken "Coolant Pump K Broken")] ;;; 122[DB4.DBX11.2]
        [("马达冲洗液压泵L故障反馈") (cons 'MotorFlushingPumpLBroken "Motor Flushing Pump L Broken")] ;;; 126[DB4.DBX11.6]
        [("冷却/马达冲洗备用泵M故障反馈") (cons 'SparePumpMBroken "Spare Pump M Broken")] ;;; 130[DB4.DBX12.2]
        [("应急液压泵Y故障反馈") (cons 'EmergencePumpYBroken "Emergence Pump Y Broken")] ;;; 134[DB4.DBX12.6]
        [("左舷闸阀冲洗泵故障反馈") (cons 'PSGVFlushingPumpBroken "PS Gate Valve Flushing Pump Broken")] ;;; 138[DB4.DBX13.2]
        [("右舷闸阀冲洗泵故障反馈") (cons 'SBGVFlushingPumpBroken "SB Gate Valve Flushing Pump Broken")] ;;; 142[DB4.DBX13.6]
        [("液压主系统油箱液位低低LS.2") (cons 'MasterTankLS2 "Master Tank Level Ultra Low")] ;;; 150[DB4.DBX14.6]
        [("耙唇液压系统油箱液位低低LS.12") (cons 'VisorTankLS2 "Drag Visor Tank Level Ultra Low")] ;;; 162[DB4.DBX16.2]
        [("A替代C") (cons 'PumpA2C "Pump A Replace C")] ;;; 176[DB4.DBX18.0]
        [("C替代A") (cons 'PumpA2C "Pump C Replace A")] ;;; 177[DB4.DBX18.1]
        [("B替代C") (cons 'PumpA2C "Pump B Replace C")] ;;; 178[DB4.DBX18.2]
        [("C替代B") (cons 'PumpA2C "Pump C Replace B")] ;;; 179[DB4.DBX18.3]
        [("F替代C") (cons 'PumpA2C "Pump F Replace C")] ;;; 180[DB4.DBX18.4]
        [("C替代F") (cons 'PumpA2C "Pump C Replace F")] ;;; 181[DB4.DBX18.5]
        [("H替代F") (cons 'PumpA2C "Pump H Replace F")] ;;; 182[DB4.DBX18.6]
        [("F替代H") (cons 'PumpA2C "Pump F Replace H")] ;;; 183[DB4.DBX18.7]
        [("G替代F") (cons 'PumpA2C "Pump G Replace F")] ;;; 184[DB4.DBX19.0]
        [("F替代G") (cons 'PumpA2C "Pump F Replace G")] ;;; 185[DB4.DBX19.1]
        [("I替代J") (cons 'PumpA2C "Pump I Replace J")] ;;; 186[DB4.DBX19.2]
        [("J替代I") (cons 'PumpA2C "Pump J Replace I")] ;;; 187[DB4.DBX19.3]
        ; [("艏吹绞车紧急停止") (cons 'ID "en_US")] ;;; 195[DB4.DBX20.3]
        ; [("左舷弯管紧急停止") (cons 'ID "en_US")] ;;; 207[DB4.DBX21.7]
        ; [("左舷耙中紧急停止") (cons 'ID "en_US")] ;;; 215[DB4.DBX22.7]
        ; [("左舷耙头紧急停止") (cons 'ID "en_US")] ;;; 223[DB4.DBX23.7]
        ; [("右舷弯管紧急停止") (cons 'ID "en_US")] ;;; 231[DB4.DBX24.7]
        ; [("右舷耙中紧急停止") (cons 'ID "en_US")] ;;; 239[DB4.DBX25.7]
        ; [("右舷耙头紧急停止") (cons 'ID "en_US")] ;;; 247[DB4.DBX26.7]
        ; [("装驳绞车紧急停止") (cons 'ID "en_US")] ;;; 255[DB4.DBX27.7]
        ; [("左舷泥泵轴承润滑单元压力低报警") (cons 'ID "en_US")] ;;; 517[DB4.DBX60.5]
        ; [("左舷泥泵轴承润滑单元液位低报警") (cons 'ID "en_US")] ;;; 518[DB4.DBX60.6]
        ; [("左舷泥泵轴承润滑单元油温高报警") (cons 'ID "en_US")] ;;; 519[DB4.DBX60.7]
        ; [("左舷泥泵轴承润滑单元水温高报警") (cons 'ID "en_US")] ;;; 520[DB4.DBX61.0]
        ; [("左舷泥泵轴承润滑单元轴承温度高1#报警") (cons 'ID "en_US")] ;;; 521[DB4.DBX61.1]
        ; [("左舷泥泵轴承润滑单元轴承温度高2#报警") (cons 'ID "en_US")] ;;; 522[DB4.DBX61.2]
        ; [("左舷泥泵轴承润滑单元轴承温度高3#报警") (cons 'ID "en_US")] ;;; 523[DB4.DBX61.3]
        ; [("右舷泥泵轴承润滑单元压力低报警") (cons 'ID "en_US")] ;;; 533[DB4.DBX62.5]
        ; [("右舷泥泵轴承润滑单元液位低报警") (cons 'ID "en_US")] ;;; 534[DB4.DBX62.6]
        ; [("右舷泥泵轴承润滑单元油温高报警") (cons 'ID "en_US")] ;;; 535[DB4.DBX62.7]
        ; [("右舷泥泵轴承润滑单元水温高报警") (cons 'ID "en_US")] ;;; 536[DB4.DBX63.0]
        ; [("右舷泥泵轴承润滑单元轴承温度高1#报警") (cons 'ID "en_US")] ;;; 537[DB4.DBX63.1]
        ; [("右舷泥泵轴承润滑单元轴承温度高2#报警") (cons 'ID "en_US")] ;;; 538[DB4.DBX63.2]
        ; [("右舷泥泵轴承润滑单元轴承温度高3#报警") (cons 'ID "en_US")] ;;; 539[DB4.DBX63.3]
        ; [("左舷水下泵1#封水泵故障反馈") (cons 'ID "en_US")] ;;; 547[DB4.DBX64.3]
        ; [("左舷水下泵2#封水泵故障反馈") (cons 'ID "en_US")] ;;; 551[DB4.DBX64.7]
        ; [("左舷泥泵齿轮箱电动滑油泵故障反馈") (cons 'ID "en_US")] ;;; 554[DB4.DBX65.2]
        ; [("左舷泥泵齿轮箱备用电动滑油泵故障反馈") (cons 'ID "en_US")] ;;; 557[DB4.DBX65.5]
        ; [("左舷泥泵齿轮箱滑油温度高报警") (cons 'ID "en_US")] ;;; 558[DB4.DBX65.6]
        ; [("左舷泥泵齿轮箱滑油压力低低报警") (cons 'ID "en_US")] ;;; 559[DB4.DBX65.7]
        ; [("右舷水下泵1#封水泵故障反馈") (cons 'ID "en_US")] ;;; 563[DB4.DBX66.3]
        ; [("右舷水下泵2#封水泵故障反馈") (cons 'ID "en_US")] ;;; 567[DB4.DBX66.7]
        ; [("右舷泥泵齿轮箱电动滑油泵故障反馈") (cons 'ID "en_US")] ;;; 570[DB4.DBX67.2]
        ; [("右舷泥泵齿轮箱备用电动滑油泵故障反馈") (cons 'ID "en_US")] ;;; 573[DB4.DBX67.5]
        ; [("右舷泥泵齿轮箱滑油温度高报警") (cons 'ID "en_US")] ;;; 574[DB4.DBX67.6]
        ; [("右舷泥泵齿轮箱滑油压力低低报警") (cons 'ID "en_US")] ;;; 575[DB4.DBX67.7]
        ; [("液压泵紧急停止(航行台)") (cons 'ID "en_US")] ;;; 700[DB4.DBX83.4]
        
        ; [("左耙中角度过大") (cons 'ID "en_US")] ;;; 3050[DB205.DBX177.2]
        ; [("右耙中角度过大") (cons 'ID "en_US")] ;;; 3051[DB205.DBX177.3]
        ; [("回油压力小于3bar,所有绞车不能动作") (cons 'ID "en_US")] ;;; 3441[DB205.DBX226.1]
        ; [("11#站ET200M-艏PLC柜第1屏通讯故障") (cons 'ID "en_US")] ;;; 3688[DB205.DBX257.0]
        ; [("12#站ET200M-艏PLC柜第2屏通讯故障") (cons 'ID "en_US")] ;;; 3689[DB205.DBX257.1]
        ; [("13#站ET200M-艏PLC柜第3屏通讯故障") (cons 'ID "en_US")] ;;; 3690[DB205.DBX257.2]
        ; [("14#站ET200M-艏PLC柜第4屏通讯故障") (cons 'ID "en_US")] ;;; 3691[DB205.DBX257.3]
        ; [("15#站ET200M-艏PLC柜第5屏通讯故障") (cons 'ID "en_US")] ;;; 3692[DB205.DBX257.4]
        ; [("16#站ET200M-疏浚台DCC1通讯故障") (cons 'ID "en_US")] ;;; 3693[DB205.DBX257.5]
        ; [("17#站ET200M-疏浚台DCC2通讯故障") (cons 'ID "en_US")] ;;; 3694[DB205.DBX257.6]
        ; [("21#站-左弯管绞车编码器通讯故障") (cons 'ID "en_US")] ;;; 3695[DB205.DBX257.7]
        ; [("22#站-右弯管绞车编码器通讯故障") (cons 'ID "en_US")] ;;; 3696[DB205.DBX258.0]
        ; [("23#站-左耙中绞车编码器通讯故障") (cons 'ID "en_US")] ;;; 3697[DB205.DBX258.1]
        ; [("24#站-右耙中绞车编码器通讯故障") (cons 'ID "en_US")] ;;; 3698[DB205.DBX258.2]
        ; [("#25#站-左耙头绞车编码器通讯故障") (cons 'ID "en_US")] ;;; 3699[DB205.DBX258.3]
        ; [("26#站-右耙头绞车编码器通讯故障") (cons 'ID "en_US")] ;;; 3700[DB205.DBX258.4]
        ; [("27#站-装驳绞车编码器通讯故障") (cons 'ID "en_US")] ;;; 3701[DB205.DBX258.5]
        ; [("2#站CPU(主)-艏PLC柜第1屏通讯故障") (cons 'ID "en_US")] ;;; 3702[DB205.DBX258.6]
        ; [("3#站CPU(备)-艏PLC柜第1屏通讯故障") (cons 'ID "en_US")] ;;; 3703[DB205.DBX258.7]
        ; [("18#站-DCS DP通讯故障") (cons 'ID "en_US")] ;;; 3704[DB205.DBX259.0]
        ; [("31#站-左舱内泵DP通讯故障") (cons 'ID "en_US")] ;;; 3705[DB205.DBX259.1]
        ; [("32#站-右舱内泵DP通讯故障") (cons 'ID "en_US")] ;;; 3706[DB205.DBX259.2]
        ; [("33#站-左高压冲水泵DP通讯故障") (cons 'ID "en_US")] ;;; 3707[DB205.DBX259.3]
        ; [("34#站-右高压冲水泵DP通讯故障") (cons 'ID "en_US")] ;;; 3708[DB205.DBX259.4]
        ; [("35#站-左水下泵DP通讯故障") (cons 'ID "en_US")] ;;; 3709[DB205.DBX259.5]
        ; [("36#站-右水下泵DP通讯故障") (cons 'ID "en_US")] ;;; 3710[DB205.DBX259.6]
        ; [("PLC电源模块电池故障") (cons 'ID "en_US")] ;;; 3711[DB205.DBX259.7]
        ; [("潮位仪通讯故障") (cons 'ID "en_US")] ;;; 3713[DB205.DBX260.1]
        ; [("GPS通讯故障") (cons 'ID "en_US")] ;;; 3714[DB205.DBX260.2]
        ; [("电罗经通讯故障") (cons 'ID "en_US")] ;;; 3715[DB205.DBX260.3]
        ; [("左耙上耙管角度过大报警") (cons 'ID "en_US")] ;;; 3920[DB205.DBX286.0]
        ; [("左耙下耙管角度过大报警") (cons 'ID "en_US")] ;;; 3921[DB205.DBX286.1]
        ; [("左耙上下耙管夹角角度过大报警") (cons 'ID "en_US")] ;;; 3922[DB205.DBX286.2]
        ; [("左耙弯管钢丝绳下放长度过大报警") (cons 'ID "en_US")] ;;; 3923[DB205.DBX286.3]
        ; [("右耙上耙管角度过大报警") (cons 'ID "en_US")] ;;; 3924[DB205.DBX286.4]
        ; [("右耙下耙管角度过大报警") (cons 'ID "en_US")] ;;; 3925[DB205.DBX286.5]
        ; [("右耙上下耙管夹角角度过大报警") (cons 'ID "en_US")] ;;; 3926[DB205.DBX286.6]
        ; [("右耙弯管钢丝绳下放长度过大报警") (cons 'ID "en_US")] ;;; 3927[DB205.DBX286.7]
        [else #false]))
    
    (let-values ([(id en_US) (if (pair? e) (values (car e) (cdr e)) (values 'ID "en_US"))])
      (unless (pair? e)
        (printf "; [(~s) (cons '~s ~s)] ;;; ~a[DB~a.DBX~a.~a]~n" zh_CN id en_US (alarm-code DB idx bidx) DB idx bidx))
      (and (pair? e) (tongue id (alarm-id DB idx bidx) en_US tokens)))))

(define main
  (lambda []
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind (λ [] (void))
                    (λ [] (let* ([/dev/txtin (open-input-file (build-path (path-only (quote-source-file)) "alarm.txt"))]
                                 [/dev/stdin (reencode-input-port /dev/txtin "GB18030")])
                            (define alarms (filter-map line-splite (port->lines /dev/stdin)))
                            (values 'alarm
                                    (filter-map (λ [line] (identify line)) alarms)
                                    1
                                    (length alarms))))
                    (λ [] (custodian-shutdown-all (current-custodian)))))))

(module+ main (main))
