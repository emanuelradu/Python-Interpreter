#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)

;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack null)
(define (make-stack) (list))

(define (push element stack) (cons element stack))
(define (top stack) (car stack))
(define (pop stack) (cdr stack))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (push co-varnames (push co-consts (push co-names (push co-code (push stack (push IC empty-stack)))))))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (top stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (top (pop stack-machine)))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (top (pop (pop stack-machine))))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (top (pop (pop (pop stack-machine)))))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (top (pop (pop (pop (pop stack-machine))))))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (top (pop (pop (pop (pop (pop stack-machine)))))))

(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (cond
    [(equal? symbol 'STACK) '4]
    [(equal? symbol 'CO-VARNAMES) '0]
    [(equal? symbol 'CO-CONSTS) '1]
    [(equal? symbol 'CO-NAMES) '2]
    [(equal? symbol 'CO-CODE) '3]
    [(equal? symbol 'INSTRUCTION-COUNTER) '5]))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (append (take stack-machine (get-symbol-index symbol)) (cons item (drop stack-machine (+ (get-symbol-index symbol) 1)))))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine (push value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

(define semne '(< <= == != > >=))

(define (semn ind)
  (car (drop semne ind)))

(define (comp sm stack)
  (cond
    ((equal? sm '<) (< (car (cdr stack)) (car stack)))
    ((equal? sm '<=) (<= (car (cdr stack)) (car stack)))
    ((equal? sm '==) (= (car (cdr stack)) (car stack)))
    ((equal? sm '!=) (not (= (car (cdr stack)) (car stack))))
    ((equal? sm '>) (> (car (cdr stack)) (car stack)))
    ((equal? sm '>=) (>= (car (cdr stack)) (car stack)))))

(define (COMPARE_OP sm stack-machine)
  (let ([stack (get-stack stack-machine)])
    (update-stack-machine (push (comp sm stack) (pop (pop stack))) 'STACK stack-machine)))

(define (POP_JUMP_IF_FALSE byte-code stack-machine)
  (if (equal? (top (get-stack stack-machine)) #f)
      (pop-exec-stack (update-stack-machine (/ byte-code 2) 'INSTRUCTION-COUNTER stack-machine))
      (pop-exec-stack stack-machine)))

(define (POP_JUMP_IF_TRUE byte-code stack-machine)
  (if (equal? (top (get-stack stack-machine)) #f)
      (pop-exec-stack (update-stack-machine (/ byte-code 2) 'INSTRUCTION-COUNTER stack-machine))
      (pop-exec-stack stack-machine)))

(define (JUMP_ABSOLUTE byte-code stack-machine)
  (update-stack-machine (/ byte-code 2) 'INSTRUCTION-COUNTER stack-machine))

(define (FOR_ITER delta stack-machine)
  (let ([stack (get-stack stack-machine)])
        (if (null? (car stack))
            (pop-exec-stack (JUMP_ABSOLUTE (+ (+ delta 2) (* (get-IC stack-machine) 2)) stack-machine))
            (update-stack-machine (cons (car (top stack)) (cons (cdr (top stack)) (pop stack))) 'STACK stack-machine))))
    
(define (RETURN_VALUE stack-machine)
  stack-machine)

(define (LOAD_CONST i s)
  (push-exec-stack (hash-ref (get-consts s) i) s))

(define (LOAD_GLOBAL nume-fct s)
  (push-exec-stack (hash-ref (get-names s) nume-fct) s))

(define (get-params nr stack)
  (if (zero? nr)
      null
      (cons (car stack) (get-params (- nr 1) (cdr stack)))))

(define (CALL_FUNCTION nr stack-machine)
  (let ([stack (get-stack stack-machine)])
    (update-stack-machine (cons (apply (get-function (car (drop stack nr))) (get-params nr stack)) (drop stack (+ nr 1))) 'STACK stack-machine)))

(define (STORE_FAST i s)
  (update-stack-machine (pop (get-stack s)) 'STACK
  (update-stack-machine (hash-set (get-varnames s) i (top (get-stack s))) 'CO-VARNAMES s)))

(define (LOAD_FAST i s)
  (push-exec-stack (hash-ref (get-varnames s) i) s))

(define (BINARY_ADD s)
  (let ([stack (get-stack s)])
    (update-stack-machine (push (+ (top (pop stack)) (top stack)) (pop (pop stack))) 'STACK s)))

(define (BINARY_SUBTRACT s)
  (let ([stack (get-stack s)])
    (update-stack-machine (push (- (top (pop stack)) (top stack)) (pop (pop stack))) 'STACK s)))

(define (BINARY_MODULO s)
  (let ([stack (get-stack s)])
    (update-stack-machine (push (remainder (top (pop stack)) (top stack)) (pop (pop stack))) 'STACK s)))

(define (alegere functie indice stack-machine)
  (cond
    ((equal? functie 'LOAD_CONST) (LOAD_CONST indice stack-machine))
    ((equal? functie 'STORE_FAST) (STORE_FAST indice stack-machine))
    ((equal? functie 'LOAD_FAST) (LOAD_FAST indice stack-machine))
    ((equal? functie 'LOAD_GLOBAL) (LOAD_GLOBAL indice stack-machine))
    ((equal? functie 'BINARY_ADD) (BINARY_ADD stack-machine))
    ((equal? functie 'BINARY_SUBTRACT) (BINARY_SUBTRACT stack-machine))
    ((equal? functie 'BINARY_MODULO) (BINARY_MODULO stack-machine))
    ((equal? functie 'INPLACE_ADD) (BINARY_ADD stack-machine))
    ((equal? functie 'INPLACE_SUBTRACT) (BINARY_SUBTRACT stack-machine))
    ((equal? functie 'INPLACE_MODULO) (BINARY_MODULO stack-machine))
    ((equal? functie 'COMPARE_OP) (COMPARE_OP (semn indice) stack-machine))
    ((equal? functie 'POP_JUMP_IF_FALSE) (POP_JUMP_IF_FALSE indice stack-machine))
    ((equal? functie 'POP_JUMP_IF_TRUE) (POP_JUMP_IF_TRUE indice stack-machine))
    ((equal? functie 'JUMP_ABSOLUTE) (JUMP_ABSOLUTE indice stack-machine))
    ((equal? functie 'SETUP_LOOP) stack-machine)
    ((equal? functie 'POP_BLOCK) stack-machine)
    ((equal? functie 'GET_ITER) stack-machine)
    ((equal? functie 'FOR_ITER) (FOR_ITER indice stack-machine))
    ((equal? functie 'CALL_FUNCTION) (CALL_FUNCTION indice stack-machine))
    ((equal? functie 'POP_TOP) (pop-exec-stack stack-machine))
    ((equal? functie 'RETURN_VALUE) (RETURN_VALUE stack-machine))))

(define (celula stack-machine)
  (car (drop (get-code stack-machine) (get-IC stack-machine))))

(define (run-stack-machine stack-machine)
  (if (= (get-IC stack-machine) (length (get-code stack-machine)))
      stack-machine
      (run-stack-machine (alegere (car (celula stack-machine)) (cdr (celula stack-machine)) (update-stack-machine (+ (get-IC stack-machine) 1) 'INSTRUCTION-COUNTER stack-machine)))))