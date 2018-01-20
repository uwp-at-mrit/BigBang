#lang scribble/manual

@(require "../handbook.rkt")

@(define-bib dark-age
   #:title    "A second dark age for C++"
   #:author   (authors "Kenny Kerr")
   #:date     "2015"
   #:url      "https://kennykerr.ca/2015/05/26/a-second-dark-age-for-c")

@handbook-story[#:style 'grouper]{浅谈微软系平台与语言}

@margin-note{我对微软系平台与语言没有系统的研究经历，
 而微软作为商业公司，并不像开源社区那样热衷于出版诸如《Unix 编程艺术》或《Unix 痛恨者手册》这样专业的文化读物。
 因此，目前本章还有很多细节需要详细考察，也有个别观点有争议，就当八卦权且一看吧。}

本章将会简单介绍微软最新的操作系统和运行时环境，如果读者对计算机文化不感兴趣可以跳过本章。
不过我依然建议@bold{不要跳过}，因为新的运行环境跟以往的 Windows 有很多本质上的差异，
过去的编程经验不一定能直接套用。这种差异不是简单的“换了一套 API”，你的思维模式也必须跟进。
或者，简单说句利益相关的话，@bold{以前的 C++ 代码不能直接用在新系统里}。

@section{Universal Windows Platform}

@section{Visual C++/CX}

@section{SCADA}

@handbook-references[]
