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


;; -- Main --

(define (scheme-interaction)
  (define (banner)
    (display "     ____        ______ __         __  ___ _     __     __ ") (newline)
    (display "    /  _/___    /_  __// /  ___   /  |/  /(_)___/ /___ / /_") (newline)
    (display "   _/ / / _ \\    / /  / _ \\/ -_) / /|_/ // // _  /(_-</ __/") (newline)
    (display "  /___//_//_/   /_/  /_//_/\\__/ /_/  /_//_/ \\_,_//___/\\__/ ") (newline))
    ;; (display "") (newline))
  
                                                                         
  ;; In order to have a REPL we must provide the connection to the REPL !
  ;; (time (test-all)) (newline)
  ;; (time (LSST_queries:example-without-printing)) (newline)
  ;; (time (QSERV_queries:example-without-printing)) (newline)
  (newline)
  (banner)
  (newline)
  (let loop ()
    (display "ITM> ")
    (let ((input (read)))
      (if (eof-object? input)
          (exit 0)
          (let ((result (eval input)))
            (and (or (boolean? result)
                     (pair? result)
                     (symbol? result)
                     (number? result)
                     (char? result)
                     (string? result)
                     (vector? result))
                 (pp result))
            (newline)
            (loop))))))

(let ((arguments (command-line)))
  (if (> (length arguments) 1)
      (let* ((sql (cadr arguments))
             (new-ast (rewriter-ast sql))
             (new-str (ast->sql new-ast)))
        (display new-ast) (newline)
        (display new-str) (newline))
      (scheme-interaction)))

;; -- End of Main -- 






