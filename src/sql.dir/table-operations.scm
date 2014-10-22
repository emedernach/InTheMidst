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
;;       table:cartesian-product
;;       table-with-default=?
;;       )

(define (table:cartesian-product table . list-of-tables)

  (define (not-empty? obj)
    (not (eq? obj 'empty-table)))
  
  (if (> (length list-of-tables) 0)
      (let* ((table-list (cons table list-of-tables))
             (table-list (keep not-empty? table-list)))
        (cartesian-product table-list))
      table))

(define (table-with-default=? default-database table_1 table_2)
  (or (equal? table_1 table_2)
      (let ((db_1   (table-record->database table_1))
            (name_1 (table-record->name table_1))
            (db_2   (table-record->database table_2))
            (name_2 (table-record->name table_2)))
        (let ((db_1 (if (eq? db_1 'Unknown) default-database db_1))
              (db_2 (if (eq? db_2 'Unknown) default-database db_2)))
          (and (string-ci=? db_1 db_2)
               (string-ci=? name_1 name_2))))))

