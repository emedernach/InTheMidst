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
;;       char-newline? extract-strings replace-if-number
;;       stream-scan string-append-list string-append-with
;;       string-search string-split string-substring?
;;       trim-whitespace
;;       )

(define string-append
  (let ((orig string-append))
    (lambda str-list
      (let check-loop ((str-list str-list))
        (or (null? str-list)
            (let ((head (car str-list))
                  (rest (cdr str-list)))
              (and (or (string? head)
                       (error "string-append not a string: " head))
                   (check-loop rest)))))
      (apply orig str-list))))

(define (string-append-list string-list)
  (let loop ((string-list string-list)
             (result '()))
    (if (null? string-list)
        (cond ((null? result) "")
              ((null? (cdr result)) (car result))
              (else (loop (reverse result) '())))
        (if (null? (cdr string-list))
            (let* ((first (car string-list))
                   (new-list (cons first result)))
              (loop (reverse new-list) '()))
            (let ((first  (list-ref string-list 0))
                  (second (list-ref string-list 1))
                  (others (list-tail string-list 2)))
              (let* ((new-string (string-append first second))
                     (new-list (cons new-string result)))
                (loop others new-list)))))))

(define (string-append-with delimiter . list-of-strings)
  (if (null? list-of-strings) ""
      (let ((head (car list-of-strings))
            (rest (cdr list-of-strings)))
        (let ((tmp (map (lambda (str)
                          (if (string=? str "") ""
                              (string-append delimiter str)))
                        rest)))
          (string-append head (apply string-append tmp))))))

;; Search a char in a string based on a predicate
(define (string-search str predicate? start)
  (let ((str-length (string-length str)))
    (let loop ((index start))
      (and (< index str-length)
           (let ((char (string-ref str index)))
             (if (predicate? char)
                 index
                 (loop (+ index 1))))))))


(define (char-not-space? ch)
  (not (or (char-whitespace? ch)
           (eq? ch #\tab))))

(define (char-quote? ch)
  (case ch
    ((#\' #\") #t)
    (else #f)))

(define (char-single-quote? ch)
  (eq? ch #\'))

(define (char-double-quote? ch)
  (eq? ch #\"))

(define (char-newline? ch)
  (eq? ch #\newline))

(define (string-substring? str substr . options)
  (define (helper start)
    (let ((str-len (string-length str))
          (substr-len (string-length substr)))
      (let ((end (+ start substr-len)))
        (and (>= str-len end)
             (string=? substr (substring str start end))))))
  (if (null? options)
      (helper 0)
      (helper (car options))))

(define (string-contains? str substr . options)
  (define (helper str substr start)
    (let ((str-len (string-length str))
          (substr-len (string-length substr)))
      (let loop ((i start))
        (let ((end (+ i substr-len)))
          (and (<= end str-len)
               (let ((str-substring (substring str i end)))
                 (if (string=? substr str-substring)
                     i (loop (+ i 1)))))))))
  (if (null? options)
      (helper str substr 0)
      (helper str substr (car options))))

;; TODO: many string  functions looks the same :  we have to
;; capture this pattern !

(define (trim-whitespace str)  
  (let ((str-length (string-length str)))
    (reverse
     (let loop ((index 0) (result '()))
       (if (< index str-length)
           (let ((char (string-ref str index)))
             (if (or (char-whitespace? char) (eq? char #\tab))
                 (let ((next (string-search str char-not-space? index)))
                   (if next
                       (loop next result)
                       result))
                 (let* ((next (string-search str char-whitespace? index))
                        (new-index (if next next str-length))
                        (substr (substring str index new-index)))
                   (if next
                       (loop new-index (cons substr result))
                       (cons substr result)))))
           result)))))

(define (replace-if-number s)
  (let ((tmp (string->number s)))
    (if tmp tmp s)))

;; TODO: Unmatched quote generates error
;; TODO: Do we support escaped quotes ?
(define (extract-strings str)

  (define (extract predicate str index result loop)
    (let ((next (string-search str predicate (+ index 1))))
      (and next 
           (let* ((substr (substring str index (+ next 1)))
                  (token (string-token substr))
                  (new-result (cons token result)))
             (loop (+ next 1) new-result)))))
  
  (let ((str-length (string-length str)))
    (reverse
     (let loop ((index 0) (result '()))
       (if (< index str-length)
           (let ((char (string-ref str index)))
             (case char
               ((#\') (extract char-single-quote?
                               str index result loop))
               ((#\") (extract char-double-quote?
                               str index result loop))
               (else
                (let ((next (string-search str char-quote? index)))
                  (if next
                      (let* ((substr (substring str index next))
                             (new-result (cons substr result)))
                        (loop next new-result))
                      (let* ((substr (substring str index str-length))
                             (new-result (cons substr result)))
                        new-result))))))
           result)))))


(define (string-split str char-list)
  
  (define (char-in-list? char)
    (memq char char-list))
  
  (let ((str-length (string-length str)))
    (reverse
     (let loop ((index 0) (result '()))
       (if (< index str-length)
           (let ((char (string-ref str index)))
             (if (char-in-list? char)
                 (loop (+ index 1) (cons (string char) result))
                 (let ((next (string-search str char-in-list? index)))
                   (if next
                       (let* ((substr (substring str index next))
                              (char-str (substring str next (+ next 1)))
                              (result (cons substr result))
                              (result (cons char-str result)))
                         (loop (+ next 1) result))
                       (cons (substring str index str-length) result)))))
           result)))))

;; For  each  string  item  in the  list  apply  a
;; function  (string -> list  of string)  and glue
;; results into a list of strings.
(define (stream-scan fun stream)
  ;; stream is a list containing strings
  (let* ((result (map (lambda (item)
                        (if (string? item)
                            (fun item)
                            item))
                      stream))
         (result (list-of-list->list result)))
    result))

(define (display-list-in-columns list-of-strings nbcolumns)
  (let loop ((i 0) (list-of-strings list-of-strings))
    (if (< i nbcolumns)
        (or (null? list-of-strings)
            (let ((head (car list-of-strings))
                  (rest (cdr list-of-strings)))
              (if (string? head)
                  (begin
                    (display "\"")
                    (display head)
                    (display "\""))
                  (display head))
              (display "  ")
              (loop (+ i 1) rest)))
        (begin
          (newline)
          (display-list-in-columns list-of-strings nbcolumns)))))

(define gensym
  (let ((num 0))
    (lambda ()
      (set! num (+ num 1))
      num)))

(define (string:benchmark)
  (define (string-append-with:benchmark)
    (let ((list-of-strings (map number->string (iota 1000))))
      (apply string-append-with ", " list-of-strings)))
  (time (begin (string-append-with:benchmark) 0)))
