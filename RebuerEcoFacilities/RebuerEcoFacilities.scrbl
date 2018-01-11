#lang scribble/manual

@(require "../Documentations/handbook.rkt")
@(require "../Documentations/graphviz.rkt")

@handbook-title[#:authors '("睿博环保设备有限公司")]{工业数据采集与监控系统手册}

@margin-note{鉴于本手册以软件工程师视角编写，我假设读者具备基础编程技能，在开发过程中能根据有效提示信息独立解决问题。}

本手册是公司软件系统的架构和设计文档，旨在帮助源码阅读者快速理解系统、融入研发，减少或消除不同专业背景同事之间的无效沟通。新同事读完本手册将有能力独立开发同类应用程序。

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
         
@handbook-smart-table[]

@;include-section[]

@handbook-appendix[#:index? #true
 (bib-entry #:key      "libmodbus"
            #:title    "A Modbus library for Linux, Mac OS X, FreeBSD, QNX and Windows"
            #:author   (authors "Stéphane Raimbault")
            #:url      "http://libmodbus.org")]
