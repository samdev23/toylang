#lang plai

(print-only-errors)

;Toy Language created by Samuel Johnson
;Basic Intepreted Functional programming to demostrate the interworkenings
;of grammars, parsers, mini compilers and interpreters


;Simple Grammar------------------------------------------------------------------------------------------------------------------------------------------
;Grammar (Uncompiled Language, includes with with statement)
(define-type raw-toylang
  ;Numbers
  [raw-num (n number?)]
  ;Addition
  [raw-add (lhs raw-toylang?)
           (rhs raw-toylang?)]
  ;Subtraction
  [raw-sub (lhs raw-toylang?)
           (rhs raw-toylang?)]
  ;Multiplication
  [raw-mult (lhs raw-toylang?)
           (rhs raw-toylang?)]
  ;Division
  [raw-div (lhs raw-toylang?)
           (rhs raw-toylang?)]
  ;Variable creation
  [raw-with (name symbol?)
            (bound-expr raw-toylang?)
            (body raw-toylang?)]
  ;Variable calling
  [raw-id (name symbol?)]
  ;Conditional statements
  [raw-if0 (cond raw-toylang?)
           (thn raw-toylang?)
           (els raw-toylang?)]
  ;Function set up
  [raw-fun (params (listof symbol?))
           (body raw-toylang?)]
  ;Function calling
  [raw-app (fun-expr raw-toylang?)
         (arg-exprs (listof raw-toylang?))]
  )


;Grammar (Compiled Language, treats with and function in same way)
(define-type toylang
  ;Numbers
  [num (n number?)]
  ;Addition
  [add (lhs toylang?)
       (rhs toylang?)]
  ;Subtraction
  [sub (lhs toylang?)
       (rhs toylang?)]
  ;Multiplication
  [mult (lhs toylang?)
        (rhs toylang?)]
  ;Division
  [div (lhs toylang?)
       (rhs toylang?)]
  ;Variable calling
  [id (name symbol?)]
  ;Add in multiplication, division, exponents
  ;Conditional statements
  [if0 (condition toylang?)
       (then toylang?)
       (else toylang?)]
  [fun (param symbol?)
       (body toylang?)]
  ;Function calling
  [app (fun toylang?)
       (arg toylang?)]
  )

;Language Helpers funcs-----------------------------------------
;Saves values produced by functions or equations
(define-type toylang-Value
  [numV (n number?)]
  [closureV (param-name symbol?)
            (body toylang?)
            (ds DefSub?)])

;Stack that saves assignments for functions and variables
(define-type DefSub
  [mtSub]
  [aSub (name  symbol?)
        (value toylang-Value?)
        (rest  DefSub?)])

;Simple Parser------------------------------------------------------------------------------------------------------------------------------------------
;Parser
(define (parse s-exp)
  (cond [(number? s-exp)
         (raw-num s-exp)]
        [(symbol? s-exp)
         (raw-id s-exp)]
        [(and (list? s-exp) (not (empty? s-exp)))
         (case (first s-exp)
           [(+)
            (check-pieces s-exp 3 "addition")
            (raw-add (parse (second s-exp))
                 (parse (third s-exp)))]
           [(-)
            (check-pieces s-exp 3 "subtraction")
            (raw-sub (parse (second s-exp))
                 (parse (third s-exp)))]
           [(*)
            (check-pieces s-exp 3 "multiplication")
            (raw-mult (parse (second s-exp))
                 (parse (third s-exp)))]
           [(/)
            (check-pieces s-exp 3 "division")
            (raw-div (parse (second s-exp))
                 (parse (third s-exp)))]
           [(with)
            (check-pieces s-exp 3 "with")
            (check-pieces (second s-exp) 2 "with binding pair")
            (raw-with (first (second s-exp))
                  (parse (second (second s-exp)))
                  (parse (third s-exp)))]
           [(if0)
            (check-pieces s-exp 4 "if0")
            (raw-if0 (parse (second s-exp))
                 (parse (third s-exp))
                 (parse (fourth s-exp)))]
           [(fun)
            (check-pieces s-exp 3 "fun")
            (raw-fun (second s-exp)
                   (parse (third s-exp)))]
           [else
            (raw-app (parse (first s-exp)) (map parse (rest s-exp)))])]
        [else
         (error 'parse "expected expression, got: ~a" s-exp)]))


;Parser Helper funcs-----------------------------------------
;Check pieces - Checks items in a list to make sure it fits the grammar
(define (check-pieces exp n expected)
  (unless (= (length exp) n)
    (error 'parse "expected ~a, got ~a" expected exp)))

;Simple Compiler----------------------------------------------------------------------------------------------------------------------------------------
(define (compile raw-toylang)
  (cond
    [(raw-num? raw-toylang) (num (raw-num-n raw-toylang))]
    [(raw-add? raw-toylang) (add (compile (raw-add-lhs raw-toylang))
                          (compile (raw-add-rhs raw-toylang)))]
    [(raw-sub? raw-toylang) (sub (compile (raw-sub-lhs raw-toylang))
                          (compile (raw-sub-rhs raw-toylang)))]
    [(raw-mult? raw-toylang) (mult (compile (raw-mult-lhs raw-toylang))
                          (compile (raw-mult-rhs raw-toylang)))]
    [(raw-div? raw-toylang) (div (compile (raw-div-lhs raw-toylang))
                          (compile (raw-div-rhs raw-toylang)))]
    [(raw-with? raw-toylang) (compile-with raw-toylang)]
    [(raw-id? raw-toylang) (id (raw-id-name raw-toylang))]
    [(raw-if0? raw-toylang) (if0 (compile (raw-if0-cond raw-toylang))
                          (compile (raw-if0-thn raw-toylang))
                          (compile (raw-if0-els raw-toylang)))]
    [(raw-fun? raw-toylang) (compile-fun raw-toylang)]
    [(raw-app? raw-toylang) (compile-app raw-toylang)]
    [else (error "Invalid w-toylang expression")]))


;Compiler Helper funcs-----------------------------------------

(define (compile-with w)
  (match w
    [(raw-with name bound-expr body-expr)
     (app (compile (raw-fun (list name) body-expr))
          (compile bound-expr))]))

(define (compile-fun f)
  (match f
    [(raw-fun params body)
     (unless (not (empty? params)) (error "nullary function"))
     (foldr (lambda (param acc)
              (fun param acc))
            (compile body)
            params)]))

(define (compile-app a)
  (match a
    [(raw-app fun-expr arg-exprs)
     (unless (not (empty? arg-exprs)) (error "nullary app"))
     (foldl (lambda (arg acc)
              (app acc (compile arg)))
            (compile fun-expr)
            arg-exprs)]))

;Simple Interpreter-------------------------------------------------------------------------------------------------------------------------------------

;Interpreter
(define (interp an-toylang ds)
  (type-case toylang an-toylang
    [num (n) (numV n)]
    [add (lhs rhs)
         (numop + lhs rhs ds)]
    [sub (lhs rhs)
         (numop - lhs rhs ds)]
    [mult (lhs rhs)
         (numop * lhs rhs ds)]
    [div (lhs rhs)
         (numop / lhs rhs ds)]
    [id (name) (lookup name ds)]
    [if0 (test true-expr false-expr)
         (define test-val (interp test ds))
         (if (or (and (numV? test-val) (= (numV-n test-val) 0))
                 (and (closureV? test-val) #f))
             (interp true-expr ds)
             (interp false-expr ds))]
    [fun (param body) (closureV param body ds)]
    [app (fun-expr arg-expr)
         (define fun-val (interp fun-expr ds))
         (define arg-val (interp arg-expr ds))
         (type-case toylang-Value fun-val
           [closureV (param-name body closed-ds)
                     (interp body
                             (aSub param-name
                                   arg-val
                                   closed-ds))]
           [else (error 'interp "not a fun")])]))

;Interpreter Helper funcs-----------------------------------------
(define (numop op l r ds)
  (define l-val (interp l ds))
  (define r-val (interp r ds))
  (unless (and (numV? l-val) (numV? r-val))
    (error 'interp "expected number"))
  (numV (op (numV-n l-val) (numV-n r-val))))

(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'interp "free")]
    [aSub (name2 val rest)
          (if (equal? name name2)
              val
              (lookup name rest))]))


;run-code function, takes your program as input-------------------------------------------------------------------------------------------------------------------------------------
(define (run-code expr)
  (let ((result (interp (compile (parse expr)) (mtSub))))
    (cond
      [(numV? result) (numV-n result)]
      [(closureV? result) 'function]
      [else (error "Invalid toylang value")])))

(define (run-toylang code)
  (run-code (read (open-input-string code))))

(define (run-toylang-loop)
  (display "Enter ToyLang code (enter 'q' to quit): ")
  (let ((input-code (read-line)))
    (unless (string=? input-code "q")
      (display "Result: ")
      (println (run-toylang input-code))
      (run-toylang-loop))))

(run-toylang-loop)



;Example programs

;(run-code `(+ 2 2))
;(run-code `(* 2 10))
;(run-code `(/ 20 5))
;(run-code `(with (x 10) (if0 x 0 1)))
;(run-code `(with (test-func (fun (y) (+ y 1))) (test-func 1)))
;(run-code `(with (y 2) y))

;(test/exn (compile (parse '(fun () (+ 1 2)))) "nullary function")
;(test/exn (compile (parse '(f ))) "nullary app")
;(test/exn (run-code '(0 Q)) "free")



;NOTE ABOUT RECURSION AND LOOPS

;Recursion and loops can be done in this language but you have to be clever. Use the mk-rec function below and place the function you want to make recursive
;in the "Code go here" space in this format:
;(with (rec-vers-of-your-func  (mk-rec (fun (your-func-name)
;                                          (fun (your-func-params)
;                                               your-func-definition)
;                                          )
;                                     )
;                             ((rec-vers-of-your-func input1 input2 etc.))))
;
;
;
;

;Make-Rec
;(run-code(with (mk-rec
;                 (fun (to-make-recursive)
;                          (with (facX (fun (facY)
;                                           (with (fac (fun (x) ((facY facY) x)))
;                                                (to-make-recursive fac))))
;                                (facX facX))
;                           ))
;                                  (Code go here)
;                                    ))

;Test recursive function
;(run-code `(with (mk-rec
;                   (fun (to-make-recursive)
;                        (with (facX (fun (facY)
;                                (with (fac (fun (x) ((facY facY) x)))
;                                     (to-make-recursive fac))))
;                                       (facX facX))
;                      ))

;                      (with (rec-test (mk-rec (fun (test) (fun (x y) (if0 y 0 (+ x (test x (- y 1))))))))
;                           (rec-test 2 2))
;                  ))