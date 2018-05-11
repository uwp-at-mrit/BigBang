#lang typed/racket

(require psd)
(require psd/profile)

(define yacht.psd : PSD
  (read-psd (build-path (find-system-path 'doc-dir)
                        "design" "3.psd")))

(psd-profile yacht.psd)
