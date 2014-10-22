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

;;      (export
;;       string-split-on-chars
;;       parser:test
;;       )

;; Simple strings lexer

;; Split around chars from char-list

;; Except that "." char may  indicate a real number, we have
;; to search for these cases. It means we cannot chop around
;; '.', we have before  to extract (space separated) numbers
;; first and chop after.

;; It  also  removes  non  significant  spaces  and  manages
;; strings.  Example:"abc \"foo  42\" def" the spaces should
;; stay inside the string !  It means that we should extract
;; strings before trimming spaces.

(define (string-split-on-chars str char-list)
  (let* ((items (extract-strings str))
         (items (stream-scan trim-whitespace items))
         (items (stream-scan replace-if-number items))
         (char-list-tmp (remove char-list '(#\. #\- #\+)))
         (items (stream-scan (lambda (str) (string-split str char-list-tmp))
                             items))
         (items (stream-scan replace-if-number items))
         (items (if (or (memq #\. char-list)
                        (memq #\- char-list)
                        (memq #\+ char-list))
                    (stream-scan (lambda (str) (string-split str char-list))
                                 items)
                    items)))
    items))





;; ---------------------------------------- ;;

(define (parser:test)
  (unit-test
   
   (equal? (string-split-on-chars "a1" '()) '("a1"))
   (equal? (string-split-on-chars "T1." '()) '("T1."))
   (equal? (string-split-on-chars "T1.*" '()) '("T1.*"))
   (equal? (string-split-on-chars "abc def" '()) '("abc" "def"))
   (equal? (string-split-on-chars "  abc   defg hijk    lm  opqr st u   " '())
           '("abc" "defg" "hijk" "lm" "opqr" "st" "u"))
   (equal? (string-split-on-chars "1 .2" '()) '(1 .2))
   (equal? (string-split-on-chars "1.2" '())  '(1.2))
   (equal? (string-split-on-chars " .4" '())  '(.4))
   (equal? (string-split-on-chars "123.45e78" '()) '(1.2345e80))
   (equal? (string-split-on-chars "-4.5" '()) '(-4.5))
   (equal? (string-split-on-chars "195,2.5" '(#\, #\.)) '(195 "," 2.5))
   
   ;; (equal? (string-split-on-chars "a.5" '())  ???
   ;; (equal? (string-split-on-chars "a5.0" '())  ???
   ;; (equal? (string-split-on-chars "a5.b" '())  ???
   ;; (equal? (string-split-on-chars "a.5b" '()) ???

   
   (equal? (string-split-on-chars "T1." '(#\.)) '("T1" "."))   
   (equal? (string-split-on-chars "T1.*" '(#\*)) '("T1." "*"))
   (equal? (string-split-on-chars "<.5" '(#\<)) '("<" .5))

   (equal? (string-split-on-chars "ps>1e-25;" '(#\> #\- #\;))
           '("ps" ">" 1e-25 ";"))

   (equal? (string-split-on-chars "123.456;" '(#\;)) '(123.456 ";"))
   (equal? (string-split-on-chars "ab.cd" '(#\, #\. #\( #\))) '("ab" "." "cd"))

   (equal?
    (string-split-on-chars "select abc , cde" '(#\, #\. #\( #\) #\;))
    '("select" "abc" "," "cde"))

   (equal? (string-split-on-chars "foo \"bar quz\" toto 42 'aze' easy" '())
           `("foo"
             ,(string-token "\"bar quz\"") "toto" 42
             ,(string-token "'aze'") "easy"))
   
   (equal?
    (string-split-on-chars
     "  abc   defg h(ijk    lm)  op.qr st, u   "
     '(#\, #\. #\( #\)))
    '("abc" "defg" "h" "(" "ijk" "lm" ")" "op" "." "qr" "st" "," "u"))

   ))

