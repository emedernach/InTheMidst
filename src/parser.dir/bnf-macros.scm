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



(define-syntax BNF-string
  (syntax-rules ()
    ((BNF-string <str>)
     (lambda (list-of-strings success failure)
       (if (null? list-of-strings)
           (failure)
           (let ((head (car list-of-strings))
                 (rest (cdr list-of-strings)))
             (if (and (string? head) (string-ci=? <str> head))
                 (success (list head) rest failure)
                 (failure))))))))

(define-syntax BNF-string-case-sensitive
  (syntax-rules ()
    ((BNF-string <str>)
     (lambda (list-of-strings success failure)
       (if (null? list-of-strings)
           (failure)
           (let ((head (car list-of-strings))
                 (rest (cdr list-of-strings)))
             (if (and (string? head) (string=? <str> head))
                 (success (list head) rest failure)
                 (failure))))))))

(define-syntax BNF-sequence
  (syntax-rules ()
    ((BNF-sequence <parser>) <parser>)
    ((BNF-sequence <parser> <other> ...)
     (lambda (list-of-strings success failure)
       (<parser>
        list-of-strings
        (lambda (match rest fallback)
          ((BNF-sequence <other> ...)
           rest
           (lambda (new-match new-rest new-fallback)
             ;;  This "append" may be costly ?  Do we need a
             ;; queue instead of a list ?
             (success (append match new-match)
                      new-rest
                      new-fallback))
           fallback))
        failure)))))

(define-syntax BNF-alternative
  (syntax-rules ()
    ((BNF-alternative <parser>) <parser>)
    ((BNF-alternative <parser> <other> ...)
     (lambda (list-of-strings success failure)
       (let ((fallback
              (lambda ()
                ((BNF-alternative <other> ...)
                 list-of-strings success failure))))
         (<parser>
          list-of-strings
          (lambda (match rest fallback)
            (success match rest fallback))
          fallback))))))

(define-syntax BNF-kleene-star
  (syntax-rules ()
    ((BNF-kleene-star <parser>)
     (letrec ((empty-parser
               (lambda (list-of-strings success failure)
                 (success '() list-of-strings failure)))
              (kleene-star
               (BNF-alternative
                empty-parser
                (BNF-sequence <parser> kleene-star))))
       kleene-star))))

(define-syntax BNF-optional
  (syntax-rules ()
    ((BNF-optional <parser>)
     (let ((empty-parser
            (lambda (list-of-strings success failure)
              (success '() list-of-strings failure))))
       (BNF-alternative empty-parser
                        <parser>)))))