#lang typed/racket

(require psd)
(require psd/profile)

(define yacht.psd : PSD
  (read-psd (build-path (find-system-path 'desk-dir)
                        "design" "1.psd")))

(psd-profile yacht.psd)
