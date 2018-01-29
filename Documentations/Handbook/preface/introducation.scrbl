#lang scribble/manual

@(require "../handbook.rkt")

@(define-bib dark-age
   #:title  "A second dark age for C++"
   #:author (authors "Kenny Kerr")
   #:date   "2015"
   #:url    "https://kennykerr.ca/2015/05/26/a-second-dark-age-for-c")

@(define-bib WinRT
   #:title  "Introducing C++/WinRT"
   #:author (authors "Kenny Kerr")
   #:date   "2017"
   #:url    "https://msdn.microsoft.com/en-us/magazine/mt745094.aspx?f=255&MSPPError=-2147217396")

@handbook-story[#:style 'grouper]{浅谈微软系平台与语言}

@margin-note{我对微软系平台与语言没有系统的研究经历，而微软作为商业公司，
 并不像开源社区那样热衷于出版诸如《Unix 编程艺术》或《Unix 痛恨者手册》这样专业的文化读物。
 因此，本章还有很多细节需要详细考察，也有个别观点有争议，目前就当八卦权且一看吧。}

完整的 @aux-elem{Windows} 的历史过于庞杂，而且跟公司系统的关系不大，我们就以 C++ 为主线说起好了。
Windows 共经历了三个大的阶段，分别是 Win32、.NET 和现在的 Universal Windows Platform。
从 2000 年到现在，C++ 在微软的地位逐渐变得尴尬起来@~cite[dark-age]，

@section{Universal Windows Platform}

@section{C++/CX}

@section{Win2D}

@handbook-references[]
