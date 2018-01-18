#lang scribble/manual

@(require "../Documentations/handbook.rkt")

@handbook-title[#:authors '("睿博环保设备有限公司")]{工业数据采集与监控系统手册}

编写本手册主要有如下几个目的
@itemlist[#:style 'compact
          @item{帮助新同事快速理解系统、融入公司，减少不同专业背景同事之间的无效沟通；}
          @item{详细说明系统开发过程中碰到的疑难杂症和解决方案;}
          @item{展示自动生成的软件质量指标；}
          @item{以及，最重要的，@bold{代码理应方便别人和将来的自己重新阅读}。}]

@margin-note{本系统所用程序语言及其分工
 @itemlist[#:style 'compact
           @item{@tt{C++} 编写完整的主程序。}
           @item{@tt{Racket} 编写所有辅助程序。如：探索系统原型、测试主程序、绘制文档中的图表、转换老代码的编码等等。}
           @item{@tt{Scribble} 编写本手册(含微调页面的 CSS)。}
           @;item{@tt{CSS} 微调 Scribble 默认的页面样式。}
           @item{@tt{ResX} 定义主程序界面的中英文字符串，由 Visual Studio 自行维护。}]}

@handbook-statistics[#:gitstat-width 420 #:gitstat-height 180
 #:ignore '(#px"/Handbook/?")
 #(C++      VioletRed    #px"[.][ch](pp|xx)?$")
 #(Racket   LimeGreen    #px"[.]rkt$")
 #(Scribble DodgerBlue   #px"[.](scrbl|css)$")
 #;(CSS     Orange       #px"[.]css$")
 #(ResX     AntiqueWhite #px"[.]resw$")]

@handbook-table[]

@include-section["../Documentations/preface.scrbl"]
@include-section["../Documentations/infrastructure.scrbl"]
@include-section["../Documentations/architecture.scrbl"]
@include-section["../Documentations/misc.scrbl"]

@handbook-appendix[#:index? #true
 (bib-entry #:key      "SE:APA"
            #:title    "Software Engineering: A Practitioner's Approach"
            #:author   (authors "Roger S. Pressman" "Bruce R. Maxim")
            #:date     "2015"
            #:location (book-location #:edition "Eighth" #:publisher "China Machine Press")
            #:is-book? #true)
 (bib-entry #:key      "libmodbus"
            #:title    "A Modbus library for Linux, Mac OS X, FreeBSD, QNX and Windows"
            #:author   (authors "Stéphane Raimbault")
            #:url      "http://libmodbus.org")]
