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



;;      (export
;;       bnf-any bnf-type bnf:test
;;       match-if-nothing-remains
;;       ok-only-if-empty
;;       )

;; Backus-Naur form parser functions

;; Abstraction  over  grammars:  we could  take  a
;; grammar  and  generate  another  one  (see  the
;; <COMMA-LIST-OF>  example which  takes  a gramar
;; and  generates a  grammar <G>  which recognizes
;; list of comma separated values of <G>)

;; grammar = lambda (list-of-strings success failure)
;; success = lambda (parsed-tokens other-tokens fallback)

;; When  a  match is  found  a  grammar calls  the
;; success function with 3 parameters: the list of
;; recognized tokens, the list of remaining tokens
;; and a fallback function which if called returns
;; another match if any.

;; NOTE:  Don't  use  the following  function  for
;; fallback, it  makes the parsing  failed all the
;; time:

;; (lambda (match rest fallback)
;;   (fallback))

;; Instead use something which verify if the match
;; or the rest is as expected.

;; (lambda (match rest fallback)
;;   (if (null? rest)
;;     match
;;     (fallback)))

;; Usage example:
;; (BNF-alternative
;;   (BNF-string "abc")
;;   (BNF-sequence
;;     (BNF-string "abc")
;;     (BNF-string "def")))


(include "bnf-macros.scm")

(define (empty-parser list-of-strings success failure)
  (success '() list-of-strings failure))

(define (Ok-only-if-empty match rest fallback)
  (if (null? rest)
      (list 'success match)
      (fallback)))

(define (match-if-nothing-remains
         match rest fallback)
  (if (null? rest)
      match
      (fallback)))

;; BNF

(define (BNF-any list-of-items success failure)
  (if (null? list-of-items)
      (failure)
      (let ((head (car list-of-items))
            (rest (cdr list-of-items)))
        (success (list head) rest failure))) )

(define (BNF-type type?)
  (lambda (list-of-strings success failure)
    (if (null? list-of-strings)
        (failure)
        (let ((head (car list-of-strings))
              (rest (cdr list-of-strings)))
          (if (type? head)
              (success (list head) rest failure)
              (failure))))))        

;; ---------------------------------------- ;;


(define (bnf:test)  
  (and

   (equal?
    (BNF-any
     '()
     (lambda args (cons 'success args))
     (lambda () 'failure))
    'failure)        

   (equal?
    (BNF-any
     '(foo)
     (lambda (match rest alt) (list 'success match))
     (lambda () 'failure))
    '(success (foo)))

   (equal?
    ((BNF-sequence
      BNF-any
      (BNF-string "foo")
      BNF-any
      (BNF-string "bar"))
     '("abc" "foo" "def" "bar")
     (lambda (match rest alt) (list 'success match))
     (lambda () 'failure))
    '(success ("abc" "foo" "def" "bar")))
   
   (equal? 
    ((BNF-string "foo")
     '("a" "b" "c")
     (lambda args (cons 'success args))
     (lambda () 'failure))
    'failure)
   
   (equal? 
    ((BNF-string "foo")
     '("foo" "a" "b" "c")
     (lambda (match rest alt) (list 'success match rest))
     (lambda ()  '(failure)))
    '(success ("foo") ("a" "b" "c")))
   
   (equal?
    ((BNF-sequence (BNF-string "foo") (BNF-string "bar"))
     '("a" "b" "c")
     (lambda args (cons 'success args))
     (lambda () 'failure))
    'failure)
   
   (equal?
    ((BNF-sequence (BNF-string "foo") (BNF-string "bar"))
     '("foo" "bar" "a" "b" "c")
     (lambda (match rest alt) (list 'success match rest))
     (lambda () 'failure))
    '(success ("foo" "bar") ("a" "b" "c")))

   (equal?
    ((BNF-alternative (BNF-string "abc") (BNF-string "xyz"))
     '("a" "b" "c")
     (lambda (match rest fallback) (list match rest))
     (lambda () 'failure))
    'failure)

   (equal?
    ((BNF-alternative (BNF-string "abc") (BNF-string "xyz"))
     '("abc" "a" "b" "c")
     (lambda (match rest fallback) (list match rest))
     (lambda () 'failure))
    '(("abc") ("a" "b" "c")))

   (equal?
    ((BNF-alternative (BNF-string "abc") (BNF-string "xyz"))
     '("xyz" "a" "b" "c")
     (lambda (match rest fallback) (list match rest))
     (lambda () 'failure))
    '(("xyz") ("a" "b" "c")))
   
   (equal?
    ((BNF-sequence
      (BNF-string "bar")
      (BNF-string "foo")
      (BNF-string "end"))
     '("bar" "foo" "end")
     (lambda (match rest fallback)
       (if (null? rest) (list 'success match) (fallback)))
     (lambda () 'failure))
    '(success ("bar" "foo" "end")))

   (equal?
    ((BNF-sequence
      (BNF-alternative
       (BNF-string "abc")
       (BNF-sequence
        (BNF-string "abc")
        (BNF-string "def")))
      (BNF-string "ghi"))
     '("abc" "def" "ghi")
     (lambda (match rest fallback) (list match rest))
     (lambda () 'failure))
    '(("abc" "def" "ghi") ()))

   (equal?
    ((BNF-kleene-star (BNF-string "abc"))
     '("a" "b" "c")
     (lambda (match rest fallback) (list match rest))
     (lambda () 'failure))
    '(() ("a" "b" "c")))

   (equal?
    ((BNF-kleene-star (BNF-string "abc"))
     '("a" "b" "c")
     (lambda (match rest fallback)
       (if (null? rest) (list 'success match) (fallback)))
     (lambda () 'failure))
    'failure)

   (equal?
    ((BNF-kleene-star (BNF-string "abc"))
     '("abc" "abc" "abc")
     (lambda (match rest fallback)
       (if (null? rest) (list 'success match) (fallback)))
     (lambda () 'failure))
    '(success ("abc" "abc" "abc")))

   (equal?
    ((BNF-kleene-star (BNF-string "abc"))
     '("abc" "a" "b" "c")
     (lambda (match rest fallback)
       (if (null? rest) (list 'success match) (fallback)))
     (lambda () 'failure))
    'failure)

   (equal?
    ((BNF-alternative
      (BNF-string "abc")
      (BNF-sequence
       (BNF-string "abc")
       (BNF-string "def")))
     '("abc" "def" "ghi")
     (lambda (match rest fallback)
       (if (> (length rest) 1)
           (fallback)
           (list 'success match rest)))
     (lambda () 'failure))
    '(success ("abc" "def") ("ghi")))

   (equal?
    ((BNF-sequence 
      (BNF-kleene-star
       (BNF-alternative
        (BNF-string "foo")
        (BNF-string "bar")))
      (BNF-string "bar")
      (BNF-string "foo")
      (BNF-string "end"))
     '("foo" "foo" "bar" "bar" "foo" "end")
     (lambda (match rest fallback)
       (if (null? rest) (list 'success match) (fallback)))
     (lambda () 'failure))
    '(success ("foo" "foo" "bar" "bar" "foo" "end")))

   (equal?
    ((BNF-sequence 
      (BNF-optional (BNF-string "foo"))
      (BNF-string "bar"))
     '("foo" "bar")
     (lambda (match rest fallback)
       (if (null? rest) (list 'success match) (fallback)))
     (lambda () 'failure))
    '(success ("foo" "bar")))

   (equal?
    ((BNF-sequence 
      (BNF-optional (BNF-string "foo"))
      (BNF-string "bar"))
     '("bar")
     (lambda (match rest fallback)
       (if (null? rest) (list 'success match) (fallback)))
     (lambda () 'failure))
    '(success ("bar")))
   
   ))
