#lang scribble/manual

@(require "../Documentations/handbook.rkt")
@(require "../Documentations/graphviz.rkt")

@handbook-story[#:style preface-style]{序: 编码风格和习惯}

@centered{@filesystem-tree[
 '(WinSCADA
   (RebuerEcoFacilities . "主程序源码")
   (BigBang . "界面和图元源码")
   (Graphics . "绘图原语源码")
   (Modbus . "通信协议源码")
   (Syslog . "日志系统源码")
   (Auxiliary . "辅助函数源码")
   ((Documentations . "文档和其他资源")
    (Handbook . "本手册的网页版")
    (Modbus . "官方规范和实现建议"))
   (#(Scripts  "辅助开发脚本" LightSlateGray)
    #(libmodbus "Modbus 协议测试套件" LightSlateGray)
    #(#&rsyslog.rkt "系统日志服务器" LightSlateGray)))]}

@handbook-references[]
