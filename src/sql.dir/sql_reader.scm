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

;; (export sql_reader)

;; SQL comments

;; "#"
;; "-- " until end of line, or "--\n"
;; Enclosed '/*' '*/' beware that it could be nested (like parenthesis) ! 

(define (SQL_reader str)

  (define (add-to-result result previous-position current-position)
    (let ((substr (substring str previous-position current-position)))
      (string-append result substr)))
  
  (define (drop-until-end-of-line result position continue)
    (let ((end-of-line (string-search str char-newline? position)))
      (if end-of-line
          (let ((next-position (+ end-of-line 1)))
            ;; TODO: how to avoid repeating 'next-position' ?
            (continue end-of-line next-position result))
          result)))

  (define (nested-comments result position continue)
    (let ((str-len (string-length str)))
      (let loop ((previous position) (position position) (result result))
        (cond ((>= position str-len) result)
              ((string-substring? str "/*" position)
               (nested-comments result (+ position 2) loop))
              ((string-substring? str "*/" position)
               (continue (+ position 2) (+ position 2) result))
              (else (loop previous (+ position 1) result))))))              
  
  (let ((str-len (string-length str)))
    (let loop ((previous 0) (position 0) (result ""))
      (cond ((>= position str-len)
             (add-to-result result previous position))
            ((or (string-substring? str "#" position)
                 (string-substring? str "-- " position)
                 (string-substring? str "--\n" position))
             (let ((result (add-to-result result previous position)))
               (drop-until-end-of-line result position loop)))
            ((string-substring? str "/*" position)
             (let ((result (add-to-result result previous position)))
               (nested-comments result (+ position 2) loop)))
            (else (loop previous (+ position 1) result))))))


(define (SQL_reader:test)
  (unit-test
   (equal? (SQL_reader "abc") "abc")
   (equal? (SQL_reader "abcd-- toto 42") "abcd")
   (equal? (SQL_reader "abc -- toto 42\n / def -- titi xyz \n 1234") 
           "abc  / def  1234")
   (equal? (SQL_reader "/****/") "")
   (equal? (SQL_reader "0 /* 1 /* 2 /* 3 */  4 */ 5 */ 6") "0  6")
   (equal? (SQL_reader "abc /* 1 2 3 */ def")
           "abc  def")
   (equal? (SQL_reader "abc /* 1 2 3 /* 4 5 6 */ 7 8 9 */ def")
           "abc  def")
   (equal? (SQL_reader "abc -/* 1 2 /****/ 3 4 5 /* 6 7 */ 8 */_ def")
           "abc -_ def")
   ))

