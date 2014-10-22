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



;; http://www.postgresql.org/docs/9.2/static/libpq.html

(c-declare "#include <libpq-fe.h>")
(c-declare "extern int pqstatus_wrapper(const PGconn *conn) ;")
(c-declare "extern PGresult *sql_select(PGconn *conn, char *select_query) ;")
(c-declare "extern int sql_command(PGconn *conn, char *select_query) ;")

;; PGconn *PQconnectdb(const char *conninfo);
(define LIBPQ:PQconnectdb
  (c-lambda (nonnull-char-string)
            (pointer "PGconn")
            "PQconnectdb"))

;; void PQfinish(PGconn *conn);
(define LIBPQ:PQfinish
  (c-lambda ((pointer "PGconn"))
            void
            "PQfinish"))

;; void PQreset(PGconn *conn);
(define LIBPQ:PQreset
  (c-lambda ((pointer "PGconn"))
            void
            "PQreset"))

;; PGPing PQping(const char *conninfo);
(define LIBPQ:PQping
  (c-lambda (nonnull-char-string)
            "PGPing"
            "PQping"))

;; ConnStatusType PQstatus(const PGconn *conn);
;; wrapped in pqstatus_wrapper
(define LIBPQ:PQstatus
  (c-lambda ((pointer "PGconn"))
            int
            "pqstatus_wrapper"))

;; We don't expose PQexec directly and wrap it with error management.

;; PGresult *PQexec(PGconn *conn, const char *command);
;; (define LIBPQ:PQexec
;;   (c-lambda ((pointer "PGconn") nonnull-char-string)
;;             (pointer "PGresult")
;;             "PQexec"))

;; (execute-sql-select connection query)
(define execute-sql-select
  (c-lambda ((pointer "PGconn") nonnull-char-string)
            (pointer "PGresult")
            "sql_select"))

;; (execute-sql-command connection query)
(define execute-sql-command
  (c-lambda ((pointer "PGconn") nonnull-char-string)
            int
            "sql_command"))

;; Asynchronous execution
;; (define execute-asynchronous-sql-select

;; http://postgresql.1045698.n5.nabble.com/libpq-PQsendQuery-wait-for-complete-result-td5734111.html#a5734321
;; PQsendQuery(conn, "SELECT 42; SELECT 'Hello'");
;; PQgetResult until NULL
(define (execute-asynchronous-sql-command connection command)
  (let ((status (LIBPQ:PQsendQuery connection command)))
    (if (= status 1)
        (let ((start (time->seconds (current-time)))
              (period 0.01))
          (let loop ((timeout period))
            (thread-sleep! (seconds->time (+ start timeout)))
            (and
             ;; #f if NULL <==> command is done
             (LIBPQ:PQgetResult connection)
             (loop (+ timeout period)))))
        (error "execute-asynchronous-sql-command: "
               (LIBPQ:PQerrorMessage)))))
  
  


;; ExecStatusType PQresultStatus(const PGresult *res);
;; TODO: to be wrappe because of enums

;; char *PQfname(const PGresult *res, int column_number);
(define LIBPQ:PQfname
  (c-lambda ((pointer "PGresult") int)
            char-string
            "PQfname"))

;; void PQclear(PGresult *res);
(define LIBPQ:PQclear
  (c-lambda ((pointer "PGresult"))
            void
            "PQclear"))

;; int PQntuples(const PGresult *res);
(define LIBPQ:PQntuples
  (c-lambda ((pointer "PGresult"))
            int
            "PQntuples"))

;; int PQnfields(const PGresult *res);
(define LIBPQ:PQnfields
  (c-lambda ((pointer "PGresult"))
            int
            "PQnfields"))

;; char *PQgetvalue(const PGresult *res, int row_number, int column_number);
(define LIBPQ:PQgetvalue
  (c-lambda ((pointer "PGresult") int int)
            char-string
            "PQgetvalue"))

;; Returns   a  single  field   value  of   one  row   of  a
;; PGresult. Row  and column numbers start at  0. The caller
;; should  not free the  result directly.  It will  be freed
;; when the associated PGresult handle is passed to PQclear.
             
;; ---- Asynchronous functions ----

;; int PQsendQuery(PGconn *conn, const char *command);
(define LIBPQ:PQsendQuery
  (c-lambda ((pointer "PGconn") nonnull-char-string)
            int
            "PQsendQuery"))

;; PQgetResult must be called  repeatedly until it returns a
;; null pointer,  indicating that  the command is  done. (If
;; called when  no command is active,  PQgetResult will just
;; return a null pointer at once.) Each non-null result from
;; PQgetResult should  be processed using  the same PGresult
;; accessor functions previously described.  Don't forget to
;; free  each  result object  with  PQclear  when done  with
;; it. Note that PQgetResult will block only if a command is
;; active and  the necessary response data has  not yet been
;; read by  PQconsumeInput.  Note: Even  when PQresultStatus
;; indicates  a fatal  error, PQgetResult  should  be called
;; until  it  returns a  null  pointer,  to  allow libpq  to
;; process the error information completely.

;; PGresult *PQgetResult(PGconn *conn);
(define LIBPQ:PQgetResult
  (c-lambda ((pointer "PGconn"))
            (pointer "PGresult")
            "PQgetResult"))

;; Submits a  command to the server without  waiting for the
;; result(s). 1 is returned  if the command was successfully
;; dispatched   and   0  if   not   (in   which  case,   use
;; PQerrorMessage   to  get   more  information   about  the
;; failure).

(define LIBPQ:PQisthreadsafe
  (c-lambda () int "PQisthreadsafe"))

(define LIBPQ:PQerrorMessage
  (c-lambda ((pointer "PGconn"))
            char-string
            "PQerrorMessage"))