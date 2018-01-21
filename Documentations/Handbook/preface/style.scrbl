#lang scribble/manual

@(require "../handbook.rkt")

@(define-bib isocpp
   #:title    "C++ Core Guidelines"
   #:author   (authors "Bjarne Stroustrup" "Herb Sutter")
   #:date     "2018"
   #:url      "http://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines")

@(define-bib JSF++
   #:title    "JOINT STRIKE FIGHTER AIR VEHICLE C++ CODING STANDARDS"
   #:author   (authors "Bjarne Stroustrup" "JSF")
   #:date     "2005"
   #:location (techrpt-location #:institution "JSF" #:number "2RDU00001 Rev C")
   #:url      "http://www.stroustrup.com/JSF-AV-rules.pdf")

@handbook-story[#:style 'grouper]{编码风格与习惯}

@section{C++/CX 编码规范}
@margin-note{完整的 C++ 编码规范可以阅读 @~cite[isocpp]，本项目文档目录中包含 @~cite[JSF++]。}

@section{源码组织结构}
@centered{@filesystem-tree[
 '(WinSCADA
   (RebuerEcoFacilities . "主程序入口源码")
   (BigBang . "界面和图元源码")
   (Graphics . "绘图原语源码")
   (Modbus . "通信协议源码")
   (Syslog . "日志系统源码")
   (Auxiliary . "辅助函数源码")
   ((Documentations . "文档和其他资源")
    (Handbook . "本手册源码")
    (Modbus . "官方规范和实现建议")
    (misc . "其他文档，如 编码规范"))
   (#(Scripts  "辅助开发脚本" LightSlateGray)
    #(libmodbus "Modbus 协议测试套件" LightSlateGray)
    #(#&rsyslog.rkt "系统日志服务器" LightSlateGray)))]}

@handbook-references[]
