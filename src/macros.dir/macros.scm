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



(define-syntax define-debug
  (syntax-rules ()
    ((define-debug (<function> <arg> ...) <body>)
     (define (<function> <arg> ...)
       (begin (display (list '<function> <arg> ...)) (newline))
       <body>))))

(define-syntax debug
  (syntax-rules ()
    ((debug <body> ...)
     '(begin <body> ...))))

(define-syntax unit-test
  (syntax-rules ()
    ((unit-test <test> ...)
     (and (or <test> (error '<test>))
          ...
          #t ))))

;;

