#lang racket

(require simple-xlsx)

(with-input-from-xlsx-file "Z:\\Desktop\\data.xlsx"
  (lambda [data.xlsx]
    (load-sheet "Sheet1" data.xlsx)
    ))
