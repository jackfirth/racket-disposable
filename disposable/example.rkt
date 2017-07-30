#lang racket/base

(require racket/contract)

(provide (contract-out [example-disposable (disposable/c (integer-in 0 99))]))

(require disposable
         racket/function)


(define example-disposable
  (disposable
   (thunk (define n (random 100)) (printf "Allocated ~v\n" n) n)
   (λ (n) (printf "Deallocated ~v\n" n))))
