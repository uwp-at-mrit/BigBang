#lang scribble/manual

@(require "../Documentations/handbook.rkt")

@handbook-title[#:authors '("睿博环保设备有限公司")]{工业数据采集与监控系统手册}

@margin-note{鉴于本手册以软件工程师视角编写，我假设读者具备基础编程技能，在开发过程中能根据有效提示信息独立解决问题。}

本手册是公司软件系统的架构和设计文档，旨在帮助源码阅读者快速理解系统、融入研发，减少或消除不同专业背景同事之间的无效沟通。新同事读完本手册将有能力独立开发同类应用程序。

@handbook-statistics[#:ignore '(#px"/Handbook$")
                     #(C++ #px"[.][ch](pp|xx)?$" VioletRed)
                     #(Racket #px"[.]rkt$" Green)
                     #(Scribble #px"[.](scrbl|css)$" Blue)]

@handbook-table[]

@include-section["../Documentations/preface.scrbl"]
@include-section["../Documentations/architecture.scrbl"]

@handbook-appendix[#:index? #true
 (bib-entry #:key      "libmodbus"
            #:title    "A Modbus library for Linux, Mac OS X, FreeBSD, QNX and Windows"
            #:author   (authors "Stéphane Raimbault")
            #:url      "http://libmodbus.org")]
