#lang scribble/manual

@(require "../Documentations/handbook.rkt")

@handbook-title[#:authors '("睿博环保设备有限公司")]{工业数据采集与监控系统手册}

@margin-note{鉴于本手册以软件工程师视角编写，我假设读者具备基础编程技能，在开发过程中能根据有效提示信息独立解决问题。}

编写本手册主要有如下几个目的
@itemlist[#:style 'compact
 @item{帮助源码阅读者快速理解系统、融入公司，减少或消除不同专业背景同事之间的无效沟通。读完本文档，新同事将有能力独立开发同类应用程序；}
 @item{详细说明系统开发过程中碰到的疑难杂症和解决方案;}
 @item{脚本化展示简单的软件质量指标和测试报告。}]

@margin-note{本系统所用开发语言及其分工
 @itemlist[#:style 'compact
           @item{@tt{C++} 开发完整的目标软件。}
           @item{@tt{Racket} 编写所有不包含在主程序中但可以提高开发效率的程序。如：探索系统原型、修正其他同事代码中的纰漏、绘制系统图表。}
           @item{@tt{Scribble} 编写本手册和文档。}
           @item{@tt{VS.resw} 目前只用来定义主程序软件界面的中英文字符串，并且由 Visual Studio 自行管理无须直接编码。}]}

@handbook-statistics[#:ignore '(#px"/Handbook/?") #:height 168
                     #(C++      VioletRed  #px"[.][ch](pp|xx)?$")
                     #(Racket   Green      #px"[.]rkt$")
                     #(Scribble DodgerBlue #px"[.](scrbl|css)$")
                     #(VS.resw  GhostWhite #px"[.](resw)$")]

@handbook-table[]

@include-section["../Documentations/preface.scrbl"]
@include-section["../Documentations/architecture.scrbl"]

@handbook-appendix[#:index? #true
 (bib-entry #:key      "libmodbus"
            #:title    "A Modbus library for Linux, Mac OS X, FreeBSD, QNX and Windows"
            #:author   (authors "Stéphane Raimbault")
            #:url      "http://libmodbus.org")]
