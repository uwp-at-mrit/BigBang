#lang scribble/manual

@(require "../Documentations/handbook.rkt")
@(require "../Documentations/graphviz.rkt")

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

@handbook-story[#:style preface-style]{0 序}

@section{浅谈微软系语言与系统}
@subsection{Universal Windows Platform}

@subsection{Visual C++/CX}

@section{编码风格与习惯}
@subsection{C++/CX 编码规范}
@margin-note{全面的 C++ 编码规范可以阅读 @~cite[isocpp]，本项目文档目录中包含 @~cite[JSF++]。}

@subsection{源码组织结构}
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
    (Modbus . "官方规范和实现建议")
    (misc . "其他文档"))
   (#(Scripts  "辅助开发脚本" LightSlateGray)
    #(libmodbus "Modbus 协议测试套件" LightSlateGray)
    #(#&rsyslog.rkt "系统日志服务器" LightSlateGray)))]}

@handbook-references[]
