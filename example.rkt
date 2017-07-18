#lang racket/base

(require racket/contract)

(provide (contract-out [example-disposable (disposable/c (integer-in 0 99))]))

(require disposable)


(define example-disposable
  (disposable/create+delete
   (λ () (define n (random 100)) (printf "Allocated ~v\n" n) n)
   (λ (n) (printf "Deallocated ~v\n" n))))
