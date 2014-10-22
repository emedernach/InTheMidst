;; Copyright (C) 2013-2014 Emmanuel Medernach
;;
;; This file is part of "In The Midst".
;;
;; "In The Midst" is  free software: you can redistribute it
;; and/or  modify  it under  the  terms  of  the GNU  Lesser
;; General Public License as  published by the Free Software
;; Foundation, either version 3  of the License, or (at your
;; option) any later version.
;; 
;; "In The Midst" is distributed in the hope that it will be
;; useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the
;; implied  warranty  of MERCHANTABILITY  or  FITNESS FOR  A
;; PARTICULAR  PURPOSE.  See the  GNU Lesser  General Public
;; License for more details.
;; 
;; You should have received a copy of the GNU Lesser General
;; Public License  along with "In  The Midst".  If  not, see
;; <http://www.gnu.org/licenses/>.

;;; Author: Emmanuel Medernach



(include "../macros.dir/macros.scm")
(include "../parser.dir/bnf-macros.scm")

;;      (export
;;       <comma-list-transformer+>
;;       <comma-list-transformer>
;;       <left-associative-transformer>
;;       bnf-type-transformer
;;       transform:test
;;       )

;; BNF parser are already (trivial) transfomer

(include "transform-macros.scm")

(define (BNF-type-transformer type? transform)
  (lambda (list-of-strings success failure)
    (if (null? list-of-strings)
        (failure)
        (let ((head (car list-of-strings))
              (rest (cdr list-of-strings)))
          (if (type? head)
              (success (list (transform head)) rest failure)
              (failure))))))

;; Utilities

;; Transform a comma list to a list

;; Contains at least one <parser>
(define (<COMMA-LIST-TRANSFORMER+> <parser>)
  (BNF-sequence-transformer
   cons
   <parser>
   (BNF-kleene-star-transformer
    '() cons
    (BNF-sequence
     (BNF-string-drop ",")
     <parser>))))

(define (<COMMA-LIST-TRANSFORMER> <parser>)
  ;; () => ()
  ;; (a) => (a)
  ;; (a , b) => (a b)
  ;; (a , b , c) => (a b c)
  (let ((empty-parser
         (lambda (list-of-strings success failure)
           (success '(()) list-of-strings failure))))
    (BNF-alternative
     empty-parser
     (<COMMA-LIST-TRANSFORMER+> <parser>))))

;; TODO: Show first how it is supposed to work with JOIN 

(define (<LEFT-ASSOCIATIVE-TRANSFORMER>
         fun <transformer-expr> <transformer-ext>)
  ;;  <transformer-expr> ( <transformer-ext> )*
  ;; Beware of the parameter order in fun :
  ;; lambda (ext result) where expr is a result
  
  (define (left-associative-transformer expr list-of-ext)
    ;; a [b] [b] ..
    (fold fun expr list-of-ext))  

  (BNF-sequence-transformer
   left-associative-transformer
   <transformer-expr>
   (BNF-kleene-star-transformer
    '() cons
    <transformer-ext>)))

;; Left associative parser
(define (<LEFT-ASSOCIATIVE-PARSER>_old fun <parser-expr> <parser-op>)
  ;; <parser-expr> (<parser-op> <parser-expr>)*
  ;; But parsed as :
  ;; a op b op c .. => (fun (fun a op b) op c) .. 

  (define (left-associative-transformer expr list-of-op-expr)
    ;; a [op b] [op c] ..
    (let op-loop ((list-of-op-expr list-of-op-expr)
                  (result expr))
      (if (null? list-of-op-expr)
          result
          (let ((head (car list-of-op-expr))
                (rest (cdr list-of-op-expr)))
            (let ((op   (vector-ref head 0))
                  (expr (vector-ref head 1)))
              (op-loop rest (fun result op expr)))))))
  
  (BNF-sequence-transformer
   left-associative-transformer
   <parser-expr>
   (BNF-kleene-star-transformer
    '() cons
    (BNF-sequence-transformer
     vector
     <parser-op>
     <parser-expr>))))



;; ---------------------------------------- ;;

(define (transform:test)
  (unit-test
   
   (equal?
    ((BNF-type-transformer symbol? symbol->string)
     '(foo)
     (lambda (match rest fallback) match)
     (lambda () 'failure))
    '("foo"))

   (equal?
    ((BNF-string-replace "foo" 'foo)
     '("foo")
     (lambda (match rest fallback) match)
     (lambda () 'failure))
    '(foo))

   (equal?
    ((BNF-sequence-transformer
      (lambda (a b)
        `((a ,a) (b ,b)))
      BNF-any BNF-any)
     '("foo" "bar")
     match-if-nothing-remains
     (lambda () 'failure))
    '(((a "foo") (b "bar"))))
   
   (equal?
    ((BNF-sequence-transformer
      + ;; Add all numbers
      (BNF-string-drop "(")
      ;; The  following kleene star returns  a list of
      ;; numbers as a match
      (BNF-kleene-star 
       (BNF-alternative
        (BNF-string-replace "0" 0)
        (BNF-string-replace "1" 1)
        (BNF-string-replace "2" 2)
        (BNF-string-replace "3" 3)
        (BNF-string-replace "4" 4)
        (BNF-string-replace "5" 5)
        (BNF-string-replace "6" 6)
        (BNF-string-replace "7" 7)
        (BNF-string-replace "8" 8)
        (BNF-string-replace "9" 9)))
      (BNF-string-drop ")"))      
     '("(" "3" "1" "4" "1" "5" "9" ")")
     (lambda (match rest fallback)
       (if (null? rest)
           (list 'success match)
           (fallback)))
     (lambda () 'failure))
    '(success (23)))

   (equal?
    ((BNF-kleene-star-transformer
      '() cons
      (BNF-string "a"))
     '("a" "a" "a" "a")
     Ok-only-if-empty
     (lambda () 'failure))
    ;; BEWARE: it returns a list of ...
    '(success ( ("a" "a" "a" "a") )))

   (equal?
    ((BNF-kleene-star-transformer
      '() cons
      (BNF-sequence
       (BNF-string-drop ",")
       (BNF-string "a")))
     '("," "a" "," "a" "," "a")
     Ok-only-if-empty
     (lambda () 'failure))
    '(success (("a" "a" "a"))))

   (equal?
    ((BNF-kleene-star->list
      (BNF-sequence-transformer
       string-append
       (BNF-alternative (BNF-string "a")
                        (BNF-string "b")
                        (BNF-string "c"))
       (BNF-alternative (BNF-string "a")
                        (BNF-string "b")
                        (BNF-string "c"))))
     '("a" "b" "c" "a" "b" "c")
     Ok-only-if-empty
     (lambda () 'failure))
    '(success (("ab" "ca" "bc"))))

   
   (equal?
    ((BNF-sequence-transformer
      (lambda (a b) (list (list 'a a) (list 'b b)))
      (BNF-string "a")
      (BNF-kleene-star-transformer
       '() cons
       (BNF-sequence
        (BNF-string-drop ",")
        (BNF-string "b"))))
     '("a" "," "b" "," "b" "," "b")
     Ok-only-if-empty
     (lambda () 'failure))
    '(success (((a "a") (b ("b" "b" "b"))))))
   
   (equal?
    ((BNF-kleene-star-transformer
      0
      (lambda (match count) (+ count 1))
      (BNF-string "a"))
     '("a" "a" "a" "a")
     Ok-only-if-empty
     (lambda () 'failure))
    '(success (4)))

   
   (equal?
    ((BNF-kleene-star-transformer
      '(0 0)
      (lambda (match count-a-b)
        (let ((count-a (car  count-a-b))
              (count-b (cadr count-a-b)))
          (cond ((string=? match "a") (list (+ count-a 1) count-b))
                ((string=? match "b") (list  count-a (+ count-b 1)))
                (else (error)))))
      (BNF-alternative (BNF-string "a") (BNF-string "b")))
     '("a" "b" "b" "a" "b")
     Ok-only-if-empty
     (lambda () 'failure))
    '(success ((2 3))))

   '(equal?
     ((BNF-sequence-transformer
       (lambda (x kleene)
         (let loop ((result x)
                    (kleene kleene))
           (if (null? kleene)
               result
               (let ((head (car kleene))
                     (rest (cdr kleene)))
                 (let ((op (car head))
                       (real (cadr head)))
                   (loop (list op result real) rest))))))       
       (BNF-type real?)
       (BNF-kleene-star-transformer
        '()
        (lambda (op real result)
          (cons (list op real) result))
        (BNF-sequence <OPERATION-TRANSFORMER> (BNF-type real?))))
      '(0 "+" 1. "-" 2. "/" 3. "*" 4.)
      Ok-only-if-empty
      (lambda () 'failure))
     '(success ((* (/ (- (+ 0 1.) 2.) 3.) 4.))))

   (letrec ((<OPERATION-TRANSFORMER>
             (BNF-alternative
              (BNF-string-replace "+" '+)
              (BNF-string-replace "-" '-)
              (BNF-string-replace "*" '*)
              (BNF-string-replace "/" '/)))
            
            (paren-expr-function
             (lambda (left op right)
               (list op left right)))
            (<PAREN-EXPR-TRANSFORMER>
             (BNF-sequence-transformer
              paren-expr-function
              (BNF-string-drop "(")
              <INFIX-NOTATION-TRANSFORMER>
              <OPERATION-TRANSFORMER>
              <INFIX-NOTATION-TRANSFORMER>
              (BNF-string-drop ")")))

            (infix-notation-function
             (lambda (expr kleene)
               (if (null? kleene)
                   expr
                   (apply (lambda (op tree)
                            (list op expr tree))
                          kleene))))
            (infix-notation-kleene-function
             (lambda (op infix tree)
               (if (null? tree)
                   (list op infix)
                   (list op infix tree))))
            (<INFIX-NOTATION-TRANSFORMER>
             (BNF-sequence-transformer
              infix-notation-function
              (BNF-alternative (BNF-type real?)
                               <PAREN-EXPR-TRANSFORMER>)
              (BNF-kleene-star-transformer
               '() infix-notation-kleene-function
               (BNF-sequence <OPERATION-TRANSFORMER>
                             <INFIX-NOTATION-TRANSFORMER>))))

            (transform-infix-notation
             (lambda (str)
               (let* ((chars '(#\( #\) #\- #\+ #\* #\/))
                      (expr (string-split-on-chars str chars)))
                 (<INFIX-NOTATION-TRANSFORMER>
                  expr
                  match-if-nothing-remains
                  (lambda () 'failure)))))
            
            )

     (equal? (transform-infix-notation "(1.2 + (2.3 * 5.6)) * (3.4 - 4.5)")
             '((* (+ 1.2 (* 2.3 5.6)) (- 3.4 4.5)))))

   ;;   A common  error is  to use  a BNF-sequence
   ;; instead  of a BNF-sequence-transformer  in a
   ;; <COMMA-LIST-TRANSFORMER>
   (equal?
    ((<COMMA-LIST-TRANSFORMER+>
      (BNF-sequence-transformer
       list
       (BNF-alternative (BNF-string "a")
                        (BNF-string "b")
                        (BNF-string "c"))
       (BNF-string "and")
       (BNF-string "next")))
     '("a" "and" "next" ","
       "b" "and" "next" ","
       "c" "and" "next")
     match-if-nothing-remains
     (lambda () 'failure))
    '((("a" "and" "next")
       ("b" "and" "next")
       ("c" "and" "next"))))

   
   (equal?
    (let* ((<ABCD>
            (BNF-alternative
             (BNF-string "a")
             (BNF-string "b")
             (BNF-string "c")
             (BNF-string "d")))
           (<OP-ABCD>
            (BNF-sequence-transformer
             vector
             (BNF-alternative
              (BNF-string "op1")
              (BNF-string "op2")
              (BNF-string "op3"))
             <ABCD>)))
      ((<LEFT-ASSOCIATIVE-TRANSFORMER>
        (lambda (op-y x)
          (let ((op (vector-ref op-y 0))
                (y  (vector-ref op-y 1)))
            (list op x y)))          
        <ABCD> <OP-ABCD>)
       '("a" "op1" "b" "op2" "c" "op3" "d")
       match-if-nothing-remains
       (lambda () 'failure)))
    '(("op3" ("op2" ("op1" "a" "b") "c") "d")))
   
   ))
