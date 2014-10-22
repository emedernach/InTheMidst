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



(define-syntax BNF-optional-transformer
  (syntax-rules ()
    ((BNF-optional-transformer <parser> <empty-value>)
     (let ((empty-parser
            (lambda (list-of-strings success failure)
              (success (list <empty-value>) list-of-strings failure))))
       (BNF-alternative empty-parser
                        <parser>)))))

;; Match its string argument and drop it
(define-syntax BNF-string-drop
  (syntax-rules ()
    ((BNF-string-drop <str>)
     (lambda (list-of-strings success failure)
       (if (or (null? list-of-strings)
               (not (pair? list-of-strings)))
           (failure)
           (let ((head (car list-of-strings))
                 (rest (cdr list-of-strings)))
             (if (and (string? head) (string-ci=? <str> head))
                 (success '() rest failure)
                 (failure))))))))

;; Match its string argument <str> and replace it with <val>
(define-syntax BNF-string-replace
  (syntax-rules ()
    ((BNF-string-replace <str> <val>)
     (lambda (list-of-strings success failure)
       (if (null? list-of-strings)
           (failure)
           (let ((head (car list-of-strings))
                 (rest (cdr list-of-strings)))
             (if (and (string? head) (string-ci=? <str> head))
                 (success (list <val>) rest failure)
                 (failure))))))))

;; Match its string argument <str> and replace it with <val>
(define-syntax BNF-string->symbol
  (syntax-rules ()
    ((BNF-string->symbol <str>)
     (let ((<symbol> (string->symbol <str>)))
       (lambda (list-of-strings success failure)
         (if (null? list-of-strings)
             (failure)
             (let ((head (car list-of-strings))
                   (rest (cdr list-of-strings)))
               (if (and (string? head) (string-ci=? <str> head))
                   (success (list <symbol>) rest failure)
                   (failure)))))))))


;; Match a  sequence, apply the transformer  function to the
;; resulting list and return the result in a list.
(define-syntax BNF-sequence-transformer
  (syntax-rules ()
    ;; <transform> is "append" in BNF-sequence
    ((BNF-sequence-transformer <transform> <parser> <other> ... )
     (lambda (list-of-strings success failure)
       ((BNF-sequence <parser> <other> ...)
        list-of-strings
        (lambda (match rest fallback)
          (success (list (apply <transform> match)) rest fallback))
        failure)))))

(define-syntax BNF-sequence->list
  (syntax-rules ()
    ((BNF-sequence->list <parser> <other> ... )
     (BNF-sequence-transformer list <parser> <other> ... ))))


;; In fact a kleene star corresponds to a fold on a list:
;; E* = Empty | E E*
(define-syntax BNF-kleene-star-transformer
  (syntax-rules ()
    ;; <transform> takes as arguments the matching and an accumulator
    ;; <zero> is an argument passed to <transform> after the matchings
    ((BNF-kleene-star-transformer <zero> <transform> <parser>)
     (letrec ((empty-parser
               (lambda (list-of-strings success failure)
                 (success (list <zero>) list-of-strings failure)))
              (kleene-star
               (BNF-alternative
                empty-parser
                (BNF-sequence-transformer <transform> <parser> kleene-star))))
       kleene-star))))

(define-syntax BNF-kleene-star->list
  (syntax-rules ()
    ((BNF-kleene-star->list <parser> <rest> ...)
     (BNF-kleene-star-transformer
      '() cons
      <parser> <rest> ...))))

(define-syntax BNF-kleene-plus->list
  (syntax-rules ()
    ((BNF-kleene-plus->list <parser> <rest> ...)
     (BNF-sequence-transformer
      cons <parser>
      (BNF-kleene-star->list <parser> <rest> ...)))))
