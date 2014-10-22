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
(include "../ast.dir/token.scm")
(include "../parser.dir/transform-macros.scm")
(include "../parser.dir/bnf-macros.scm")

;;      (export
;;       <arithmetic>
;;       <case-else-expr>
;;       <case-expr>
;;       <case-value>
;;       <case-when-expr>
;;       <comparator>
;;       <constant>
;;       <crossmatch>
;;       <field-expr-atom>
;;       <field-expr>
;;       <field-test-equality>
;;       <field>
;;       <from>
;;       <function-call>
;;       <group-by>
;;       <having>
;;       <join-table-with-parenthesis>
;;       <limit>
;;       <logic>
;;       <order-by>
;;       <predicate>
;;       <select-field-expr>
;;       <select>
;;       <sql-selection>
;;       <sql-with-optional-parenthesis>
;;       <string>
;;       <subquery>
;;       <table-alias>
;;       <table-with-optional-join>
;;       <table>
;;       <where>
;;
;;       sql->ast sql_compiler:test)

;; From the SQL BNF we should be able to describe a SQL compiler

;; For that we could use a macro

;; But for the moment we will write it from the BNF

;; TODO:
;; - "SELECT TOP <N>" is MySQL specific, do we support it or not ? 
;; - Qualified functions such as "db.fun(...)" are not supported.

(define (sql-split str)
  ;; - We should extract quote delimited strings
  ;; - "<>" is read as '("<" ">")
  (let* ((chars '(#\" #\' #\, #\. #\( #\) #\;
                  #\- #\+ #\* #\/ #\= #\% #\> #\<))
         (split (string-split-on-chars str chars)))
    split))

(define <TABLE-COMBINATOR>
  (BNF-alternative
   (BNF-string "UNION")
   (BNF-sequence-transformer
    (lambda () "UNION ALL")
    (BNF-string-drop "UNION")
    (BNF-string-drop "ALL"))   
   (BNF-string "INTERSECT")
   (BNF-sequence-transformer
    (lambda () "INTERSECT ALL")
    (BNF-string-drop "INTERSECT")
    (BNF-string-drop "ALL"))
   (BNF-string "EXCEPT")
   (BNF-sequence-transformer
    (lambda () "EXCEPT ALL")
    (BNF-string-drop "EXCEPT")
    (BNF-string-drop "ALL")))) 

;; Note  we  cannot extend  SQL  with  function call  taking
;; SELECT as a parameter because of commas (,) in table list
;; confusing commas in parameter list.

(define <SQL>
  (BNF-sequence
   (BNF-alternative
    (BNF-sequence-transformer
     sql-transformer
     <SQL-WITH-OPTIONAL-PARENTHESIS>
     (BNF-kleene-star->list
      (BNF-sequence-transformer
       table-combinator-transformer
       <TABLE-COMBINATOR>
       <SQL-WITH-OPTIONAL-PARENTHESIS>))))
   (BNF-optional (BNF-string-drop ";"))))

(define <SQL-WITH-PARENTHESIS>
  (BNF-sequence-transformer
   make-parenthesised-expression
   (BNF-string-drop "(")
   <SQL>  
   (BNF-string-drop ")")))

(define <SQL-WITH-OPTIONAL-PARENTHESIS>
  (BNF-alternative
   <SQL-SELECTION>  
   <SQL-WITH-PARENTHESIS>))

(define <SQL-SELECTION>  
  (BNF-sequence-transformer
   sql-selection-transformer
   <SELECT> 
   (BNF-optional-transformer <FROM> 'empty-from)
   ;; CROSSMATCH extension
   (BNF-optional-transformer <CROSSMATCH> 'empty-crossmatch)
   (BNF-optional-transformer <WHERE> 'empty-where)
   (BNF-optional-transformer <GROUP-BY> 'empty-group)
   (BNF-optional-transformer <ORDER-BY> 'empty-order-by)
   (BNF-optional-transformer <LIMIT> 'empty-limit)     
   ))

(define <SELECT>
  ;; Returns a projection (table -> table)
  (BNF-sequence-transformer
   select-transformer
   (BNF-string-drop "SELECT")
   (BNF-optional-transformer (BNF-string "DISTINCT") #f)
   (<COMMA-LIST-TRANSFORMER+> <SELECT-FIELD-EXPR>)))

(define <SELECT-FIELD-EXPR>
  (BNF-sequence-transformer
   select-field-transformer
   <FIELD-EXPR>
   (BNF-optional-transformer
    (BNF-sequence
     (BNF-optional (BNF-string-drop "AS"))
     (BNF-alternative (BNF-type string?) <STRING>))
    'no-alias)))

(define <CAST-EXPR>
  (BNF-sequence-transformer
   cast-expr-transformer
   (BNF-string-drop "CAST")
   (BNF-string-drop "(")
   <FIELD-EXPR>
   (BNF-string-drop "AS")
   (BNF-type string?)
   (BNF-string-drop ")")))

;; AVG MIN MAX SUM COUNT are viewed as functions.
(define <FIELD-EXPR>
  ;; Same as <PREDICATE>, done like this to preserve ordering in rewriting.
  (BNF-sequence-transformer
   field-expr-transformer
   <FIELD-EXPR-ATOM>
   (BNF-kleene-star->list
    (BNF-sequence-transformer
     partial-arithmetic
     <ARITHMETIC> <FIELD-EXPR>))))

(define <STRING> (BNF-type string-token?))

(define <FIELD-EXPR-ATOM>
  (BNF-alternative
   (BNF-sequence-transformer
    unary-arithmetic
    (BNF-string->symbol "-")
    <FIELD>)
   <FIELD>
   <STRING>      
   <CAST-EXPR>
   <FUNCTION-CALL>
   <CASE-EXPR>
   <CASE-VALUE>
   (BNF-sequence-transformer
    make-parenthesised-expression
    (BNF-string-drop "(")
    <FIELD-EXPR>
    (BNF-string-drop ")"))

   <SQL>
   
   (BNF-sequence-transformer
    make-parenthesised-expression
    (BNF-string-drop "(")
    <SQL>
    (BNF-string-drop ")"))))

(define <FIELD>
  (BNF-alternative
   (BNF-type number?)
   <STRING>
   ;; In  the case below we  need to find on  which table is
   ;; this  field defined.  BEWARE  the table name  could be
   ;; defined later  as an alias so we  cannot lookup for it
   ;; in the  current environment ! We need  to do this when
   ;; all the expressions are parsed.
   (BNF-type-transformer string? field-name-transformer)
   (BNF-sequence-transformer
    field
    (BNF-type string?)
    (BNF-string-drop ".")
    (BNF-type string?))))

(define <FUNCTION-CALL>
  (BNF-sequence-transformer
   function-call
   (BNF-type string?)
   (BNF-string-drop "(")
   (<COMMA-LIST-TRANSFORMER> <FIELD-EXPR>)
   (BNF-string-drop ")")))

(define <ANY-EXPR>
  (BNF-sequence-transformer
   any-expr-transformer
   (BNF-string-drop "ANY")
   (BNF-string-drop "(")
   <SQL>
   (BNF-string-drop ")")))

(define <ALL-EXPR>
  (BNF-sequence-transformer
   all-expr-transformer
   (BNF-string-drop "ALL")
   (BNF-string-drop "(")
   <SQL>
   (BNF-string-drop ")")))

(define <SOME-EXPR>
  (BNF-sequence-transformer
   some-expr-transformer
   (BNF-string-drop "SOME")
   (BNF-string-drop "(")
   <SQL>
   (BNF-string-drop ")")))

(define <PREDICATE-EXPR>
  (BNF-alternative

   (BNF-sequence-transformer
    make-parenthesised-expression
    (BNF-string-drop "(")
    <PREDICATE>
    (BNF-string-drop ")"))

   (BNF-sequence-transformer
    exists-transformer
    (BNF-string-drop "EXISTS")
    (BNF-string-drop "(")
    <SQL>
    (BNF-string-drop ")"))
   
   <FIELD-EXPR>
   
   (BNF-sequence-transformer
    comparison-transformer
    <FIELD-EXPR>
    <COMPARATOR>
    (BNF-alternative
     <ANY-EXPR>
     <ALL-EXPR>
     <SOME-EXPR>
     <FIELD-EXPR>))

   (BNF-sequence-transformer
    like-transformer
    <FIELD-EXPR>
    (BNF-optional-transformer
     (BNF-string-replace "NOT" 'not)
     'nothing)
    (BNF-string-drop "LIKE")
    <STRING>)
   
   (BNF-sequence-transformer
    is-null-transformer
    <FIELD>
    (BNF-string-drop "IS")
    (BNF-optional-transformer
     (BNF-string-replace "NOT" 'not)
     'nothing)
    (BNF-string-drop "NULL"))
   
   (BNF-sequence-transformer
    between-predicate-transformer
    <FIELD-EXPR>
    (BNF-optional-transformer (BNF-string "NOT") "")
    (BNF-string-drop "BETWEEN")
    <FIELD-EXPR>
    (BNF-string-drop "AND")
    <FIELD-EXPR>)

   (BNF-sequence-transformer
    set-predicate-transformer
    <FIELD-EXPR>
    (BNF-optional-transformer (BNF-string "NOT") "")
    (BNF-string-drop "IN")
    (BNF-alternative
     ;; Set of values
     (BNF-sequence-transformer
      list->set
      (BNF-string-drop "(")
      (<COMMA-LIST-TRANSFORMER+> <CONSTANT>)
      (BNF-string-drop ")"))

     ;; Subquery
     (BNF-sequence-transformer
      make-parenthesised-expression
      (BNF-string-drop "(") <SQL> (BNF-string-drop ")")))
    )))

(define <PREDICATE-WITH-OPTIONAL-NEGATION>
  (BNF-sequence-transformer
   predicate-with-optional-negation-transformer
   (BNF-optional-transformer (BNF-string "NOT") "")
   <PREDICATE-EXPR>))

(define <PREDICATE>
  ;; SQL ambiguity here: a AND b OR c (Where to put parenthesis ?)
  ;; It seems to be (a AND b) or c
  (BNF-sequence-transformer
   ;; WARNING:  with LEFT ASSOCIATIVE ?  No:  For the moment
   ;;  we  do  like  this  in order  to  preserve  predicate
   ;; ordering in rewriting.
   predicate-transformer
   <PREDICATE-WITH-OPTIONAL-NEGATION>
   (BNF-kleene-star->list
    (BNF-sequence-transformer
     partial-predicate
     <LOGIC>
     <PREDICATE-WITH-OPTIONAL-NEGATION>
     ))))

;; ======================================== ;;

(define <CASE-EXPR>
  (BNF-sequence-transformer
   case-expr-transformer
   (BNF-string-drop "CASE")
   (BNF-kleene-star->list <CASE-WHEN-EXPR>)
   (BNF-optional-transformer <CASE-ELSE-EXPR> 'no-else)
   (BNF-string-drop "END")))

(define <CASE-VALUE>
  (BNF-sequence-transformer
   case-value-transformer
   (BNF-string-drop "CASE")
   <FIELD-EXPR>
   (BNF-kleene-star->list <CASE-WHEN-EXPR>)
   (BNF-optional-transformer <CASE-ELSE-EXPR> 'no-else)
   (BNF-string-drop "END")))

(define <CASE-WHEN-EXPR>
  (BNF-sequence-transformer
   case-when-expr-transformer
   (BNF-string-drop "WHEN")
   <PREDICATE>
   (BNF-string-drop "THEN")
   <SELECT-FIELD-EXPR>))

(define <CASE-ELSE-EXPR>
  (BNF-sequence-transformer
   case-else-expr-transformer
   (BNF-string-drop "ELSE")
   <SELECT-FIELD-EXPR>))


;; ======================================== ;;

(define <CONSTANT>
  (BNF-alternative
   (BNF-type number?)
   (BNF-type string?)))

;; Arithmetic

(define <ARITHMETIC>
  (BNF-alternative
   (BNF-string->symbol "+")
   (BNF-string->symbol "-")
   (BNF-string->symbol "*")
   (BNF-string->symbol "/")
   (BNF-string->symbol "%") ;; modulo
   (BNF-string->symbol "|")
   (BNF-string->symbol "&")
   ))

;; CROSSMATCH extension:

;; SELECT *
;; FROM Object O, Source S
;; CROSSMATCH (O, ra_PS, decl_PS) 
;;        AND (S, ra, decl)
;; WITH RADIUS 0.0001
;; WHERE ...

(define <CROSSMATCH>
  (BNF-kleene-plus->list
   (BNF-sequence-transformer
    crossmatch-transformer
    (BNF-string-drop "CROSSMATCH")
    (BNF-string-drop "(")
    (BNF-type string?) (BNF-string-drop ",")
    (BNF-type string?) (BNF-string-drop ",")
    (BNF-type string?) 
    (BNF-string-drop ")")
    (BNF-string-drop "AND")
    (BNF-string-drop "(")
    (BNF-type string?) (BNF-string-drop ",")
    (BNF-type string?) (BNF-string-drop ",")
    (BNF-type string?) 
    (BNF-string-drop ")")
    (BNF-string-drop "WITH")
    (BNF-string-drop "RADIUS")
    (BNF-type number?))))

;; Tables

(define <FROM>
  (BNF-sequence-transformer
   from-transformer
   (BNF-string-drop "FROM")
   (<COMMA-LIST-TRANSFORMER+> <TABLE>)))

(define <TABLE>
  (BNF-alternative
   <SUBQUERY>
   <TABLE-WITH-OPTIONAL-JOIN>
   ))

(define <SUBQUERY>
  (BNF-sequence-transformer
   subquery-transformer
   (BNF-sequence-transformer
    make-parenthesised-expression
    (BNF-string-drop "(") <SQL> (BNF-string-drop ")"))
   ;; SQL disallows anonymous tables 
   (BNF-string-drop "AS")
   (BNF-alternative (BNF-type string?) <STRING>)))

(define <RAW-TABLE>
  (BNF-alternative
   
   ;; Table string name
   (BNF-type-transformer string? table-name-transformer)      
   
   ;; DB.table
   (BNF-sequence-transformer
    database-table-name-transformer
    (BNF-type string?)
    (BNF-string-drop ".")
    (BNF-type string?))

   ;; Function call returning a table
   <FUNCTION-CALL>  
   
   <SQL-WITH-PARENTHESIS>
   
   <JOIN-TABLE-WITH-PARENTHESIS>
   ))

(define <JOIN-TABLE-TYPE>
  (BNF-alternative
   (BNF-string->symbol "inner")
   (BNF-string->symbol "left")
   (BNF-string->symbol "right")
   (BNF-string->symbol "full")
   ))

(define <JOIN-TABLE-USING>
  (BNF-sequence-transformer
   join-table-using-transformer
   (BNF-optional-transformer <JOIN-TABLE-TYPE> 'INNER)
   (BNF-optional (BNF-string-drop "OUTER"))
   (BNF-string-drop "JOIN") <TABLE>
   (BNF-string-drop "USING")
   (BNF-string-drop "(")
   (<COMMA-LIST-TRANSFORMER+> <FIELD>)
   (BNF-string-drop ")")))

(define <JOIN-TABLE-ON>
  (BNF-sequence-transformer
   join-table-on-transformer
   (BNF-optional-transformer <JOIN-TABLE-TYPE> 'INNER)
   (BNF-string-drop "JOIN") <TABLE>
   (BNF-string-drop "ON")
   (BNF-alternative
    <FIELD-TEST-EQUALITY>
    (BNF-sequence
     (BNF-string-drop "(")
     <FIELD-TEST-EQUALITY>
     (BNF-string-drop ")")))))

(define <JOIN-TABLE>
  (BNF-alternative
   <JOIN-TABLE-USING>
   <JOIN-TABLE-ON>))

(define <JOIN-TABLE-WITH-PARENTHESIS>
  (BNF-sequence-transformer
   make-parenthesised-expression
   (BNF-string-drop "(")
   <TABLE-WITH-OPTIONAL-JOIN>  
   (BNF-string-drop ")")))

(define <TABLE-WITH-OPTIONAL-ALIAS>
  (BNF-alternative
   <RAW-TABLE>
   <TABLE-ALIAS>))

(define <TABLE-WITH-OPTIONAL-JOIN>
  (<LEFT-ASSOCIATIVE-TRANSFORMER>
   table-with-optional-join-transformer
   <TABLE-WITH-OPTIONAL-ALIAS>
   <JOIN-TABLE>))

(define <TABLE-ALIAS>
  (BNF-sequence-transformer
   table-alias-transformer
   <RAW-TABLE>
   (BNF-optional (BNF-string-drop "AS"))
   (BNF-alternative (BNF-type string?) <STRING>)))



;; Predicates

;; A AND B OR C AND D is interpreted as (A AND B) OR (C AND D)
;; i.e. It is sufficient to keep a list of predicates
;; and to put parenthesis around OR
(define <LOGIC>
  (BNF-alternative
   (BNF-string "AND")
   (BNF-string "OR")))

(define <COMPARATOR>
  (BNF-alternative
   (BNF-string "=")
   (BNF-string ">")
   (BNF-string "<")

   ;; These strings pose a problem because we split around
   ;; chars (<, >, =), we replace this with 
   (BNF-string ">=")     
   (BNF-string "<=")
   (BNF-string "<>")
   (BNF-string "!=")

   (BNF-sequence-transformer
    (lambda args ">=")
    (BNF-string ">") (BNF-string "="))
   (BNF-sequence-transformer
    (lambda args "<=")
    (BNF-string "<") (BNF-string "="))
   (BNF-sequence-transformer
    (lambda args "<>")
    (BNF-string "<") (BNF-string ">"))
   (BNF-sequence-transformer
    (lambda args "!=")
    (BNF-string "!") (BNF-string "="))
   
   ))

(define <FIELD-TEST-EQUALITY>
  (BNF-sequence-transformer
   field-test-equality-transformer
   <FIELD-EXPR>
   (BNF-string-drop "=") 
   <FIELD-EXPR>))

;; ---------------------------------------- ;;

;; TODO
;; From DB-BOOK :
;; Attributes in select clause outside of aggregate functions must appear in group by list
;; /* erroneous query */ select dept_name, ID, avg (salary) from instructor group by dept_name;

(define <GROUP-BY>
  (BNF-sequence-transformer
   group-by-transformer
   (BNF-string-drop "GROUP")
   (BNF-string-drop "BY")
   (<COMMA-LIST-TRANSFORMER+>
    (BNF-alternative
     <FIELD-EXPR>
     (BNF-sequence
      (BNF-string-drop "(")
      <FIELD-EXPR>
      (BNF-string-drop ")"))))
   (BNF-optional-transformer <HAVING> #t)))

(define <ORDER-BY>
  (BNF-sequence-transformer
   order-by-transformer
   (BNF-string-drop "ORDER")
   (BNF-string-drop "BY")
   (<COMMA-LIST-TRANSFORMER+> <FIELD-EXPR>)
   (BNF-optional-transformer
    (BNF-alternative
     (BNF-string "ASC")
     (BNF-string "DESC"))
    "ASC")))

(define <LIMIT>
  (BNF-sequence-transformer
   limit-transformer
   (BNF-string-drop "LIMIT")
   (BNF-type integer?)))

(define <WHERE>
  (BNF-sequence-transformer
   where-transformer
   (BNF-string-drop "WHERE")
   <PREDICATE>))

(define <HAVING>
  (BNF-sequence
   (BNF-string-drop "HAVING")
   <PREDICATE>))

;; ---------------------------------------- ;;

(define (SQL->AST str)
  (let ((sql-str (SQL_reader str)))
    (let ((result (<SQL> (sql-split sql-str)
                         Ok-only-if-empty
                         (lambda () 'failure))))
      (if (and (pair? result)
               (eq? (car result) 'success))
          (let ((ast (caadr result))) ast)
          (error "String not successfully parsed as SQL" str)))))


;; ---------------------------------------- ;;

;; Warning : "SELECT a FROM (SELECT * FROM T) ;"
;; is not valid SQL because subqueries must be named as 
;; "SELECT a FROM (SELECT * FROM T) AS T2;"

(define (sql_compiler:test)
  (unit-test
   
   (equal?
    (<FIELD> '("a") Ok-only-if-empty (lambda () 'failure))
    `(success (,(field 'Unknown "a"))))

   (equal?
    (<FIELD> '("a" "." "b") Ok-only-if-empty (lambda () 'failure))
    `(success (,(field "a" "b"))))
   
   (equal?
    ((<COMMA-LIST-TRANSFORMER> (BNF-string "a")) 
     '("a" "," "a" "," "a")
     Ok-only-if-empty
     (lambda () 'failure))
    '(success (("a" "a" "a"))))

   (equal?
    (<SELECT> '("SELECT") Ok-only-if-empty (lambda () 'failure))
    'failure)

   (equal?
    (<SELECT> '("SELECT" "(" "T" "." "foo" ")") Ok-only-if-empty (lambda () 'failure))
    `(success
      (,(projection #f (list (field "T" "foo"))))))

   (equal? (<SELECT> '("SELECT" "-" "a") Ok-only-if-empty (lambda () 'failure))
           `(success (,(projection #f (list (unary-arithmetic '- (field 'Unknown "a")))))))
   
   (equal?
    (<SELECT> '("SELECT" "-" "T" "." "foo") Ok-only-if-empty (lambda () 'failure))
    `(success
      (,(projection
         #f
         (list
          (unary-arithmetic '- (field "T" "foo")))))))
   
   (equal?
    (<SELECT> '("SELECT" "a" "," 1 "+" 2 "-" 3)  Ok-only-if-empty (lambda () 'failure))
    `(success
      (,(projection
         #f
         (list
          (field 'Unknown "a")
          (arithmetic 1 (list (partial-arithmetic '+ 2)
                              (partial-arithmetic '- 3)
                              )))))))
   
   (equal?
    (<SELECT>
     '("SELECT" "a" "," "foo" "." "bar" "," "fun" "(" "x" ")")
     Ok-only-if-empty (lambda () 'failure))
    `(success
      (,(projection
         #f
         (list (field 'Unknown "a")
               (field "foo" "bar")
               (function-call
                "fun"
                (list (field 'Unknown "x"))))))))
   
   (equal?
    (<FUNCTION-CALL>
     '("foo" "(" "a" "," "b" "," 37 "," "tab" "." "xyz" "," "c" ")")
     Ok-only-if-empty
     (lambda () 'failure))
    `(success
      (,(function-call
         "foo"
         (list (field 'Unknown "a")
               (field 'Unknown "b")
               37
               (field "tab" "xyz")
               (field 'Unknown "c"))))))

   (equal?
    (<SELECT> '("SELECT" "a") Ok-only-if-empty (lambda () 'failure))
    `(success (,(projection #f (list (field 'Unknown "a"))))))

   (equal?
    (<SELECT> '("SELECT" "a" "," "T" "." "b")
              Ok-only-if-empty (lambda () 'failure))
    `(success (,(projection #f (list (field 'Unknown "a")
                                     (field "T" "b"))))))

   (equal?
    (<FROM> '("FROM" "Table1")
            Ok-only-if-empty (lambda () 'failure))
    `(success (,(table-record 'Unknown "Table1"))))

   (equal?
    (<FROM> '("FROM" "Table1" "," "Table2")
            Ok-only-if-empty (lambda () 'failure))
    `(success (,(table:cartesian-product
                 (table-record 'Unknown "Table1")
                 (table-record 'Unknown "Table2")))))
   
   (equal?
    (<FROM>
     '("FROM" "Table1" "JOIN" "Table2" "ON" "id1" "=" "id2")
     Ok-only-if-empty (lambda () 'failure))
    `(success
      (,(join-table-on
         'INNER
         (table-record 'Unknown "Table1")
         (table-record 'Unknown "Table2")
         (comparison
          "="
          (field 'Unknown "id1")
          (field 'Unknown "id2"))))))

   (equal?
    (<PREDICATE-EXPR>
     '("some_boolean")
     Ok-only-if-empty
     (lambda () 'failure))
    `(success (,(field 'Unknown "some_boolean"))))
   
   (equal?
    (<PREDICATE-EXPR>
     '("(" "some_boolean" ")")
     Ok-only-if-empty
     (lambda () 'failure))
    `(success (,(field 'Unknown "some_boolean"))))

   (equal?
    (<PREDICATE-EXPR>
     '("foo" "=" 42)
     Ok-only-if-empty
     (lambda () 'failure))
    `(success (,(comparison "=" (field 'Unknown "foo") 42))))

   (equal?
    (<PREDICATE-EXPR>
     '("(" "foo" "=" 42 ")")
     Ok-only-if-empty
     (lambda () 'failure))
    `(success (,(parenthesised-expression (comparison "=" (field 'Unknown "foo") 42)))))

   (equal?
    (<PREDICATE-WITH-OPTIONAL-NEGATION>
     '("foo" "=" 42)
     Ok-only-if-empty
     (lambda () 'failure))
    `(success (,(comparison "=" (field 'Unknown "foo") 42))))

   (equal?
    (<PREDICATE-WITH-OPTIONAL-NEGATION>
     '("NOT" "foo" "=" 42)
     Ok-only-if-empty
     (lambda () 'failure))
    `(success (,(negation-predicate (comparison "=" (field 'Unknown "foo") 42)))))

   (equal?
    (<CASE-ELSE-EXPR>
     '("ELSE" 100)
     Ok-only-if-empty
     (lambda () 'failure))
    `(success (,(else-clause 100))))

   (equal?
    (<PREDICATE>
     '("(" "foo" "=" 42 ")")
     Ok-only-if-empty
     (lambda () 'failure))
    `(success (,(parenthesised-expression (comparison "=" (field 'Unknown "foo") 42)))))

   ;; We cannot compare sets with 'equal?' 
   (let ((predicate-test
          (<PREDICATE> '("val" "IN" "(" 1 "," 2 "," 3 "," 5 "," 8 ")")
                       Ok-only-if-empty
                       (lambda () 'failure))))
     (and (list? predicate-test)
          (eq? (car predicate-test) 'success)
          (let ((predicate-test (caadr predicate-test)))
            (and (set-predicate? predicate-test)
                 (equal? (set-predicate->expr predicate-test)
                         (field 'Unknown "val"))
                 (set-equal? (set-predicate->set-of-values predicate-test)
                             (make-set 1 2 3 5 8))))))
   
   (equal?
    (<CASE-WHEN-EXPR>
     '("WHEN" "(" "foo" "=" 42 ")" "THEN" 123)
     Ok-only-if-empty
     (lambda () 'failure))
    `(success (,(case-clause
                 (parenthesised-expression (comparison "=" (field 'Unknown "foo") 42))
                 123))))
   
   (equal?
    (<CASE-EXPR>
     '("CASE" "WHEN" "(" "typeId" "=" 3 ")" "THEN" 1 "END")
     Ok-only-if-empty (lambda () 'failure))
    `(success (,(case-expr
                 (list (case-clause (parenthesised-expression (comparison "=" (field 'Unknown "typeId") 3)) 1))
                 'no-else))))

   (equal?
    (<CASE-EXPR>
     '("CASE" "WHEN" "(" "typeId" "=" 3 ")" "THEN" 1 "ELSE" 0 "END")
     Ok-only-if-empty (lambda () 'failure))
    `(success
      (,(case-expr
         (list (case-clause (parenthesised-expression (comparison "=" (field 'Unknown "typeId") 3)) 1))
         (else-clause 0)))))

   (equal?
    (<SELECT> '("SELECT"
                "a" "," "b" "as" "b_" ","
                37 "," "tab" "." "xyz" ","
                "c" "(" ")" ","
                "d" "(" "x" "," "y" "," 42 ")")
              Ok-only-if-empty (lambda () 'failure))
    `(success
      (,(projection
         #f
         (list (field 'Unknown "a")
               (make-alias "b_" (field 'Unknown "b"))
               37
               (field "tab" "xyz")
               (function-call "c" '())
               (function-call "d" (list (field 'Unknown "x")
                                        (field 'Unknown "y")
                                        42)))))))   
   
   (equal?
    (<PREDICATE>
     '("foo" "=" 42 "AND"
       "bar" ">" 0 "OR"
       "bool")
     Ok-only-if-empty
     (lambda () 'failure))
    `(success
      (,(predicate
         (comparison "=" (field 'Unknown "foo") 42)
         (list (partial-predicate "AND" (comparison ">" (field 'Unknown "bar") 0))
               (partial-predicate "OR"  (field 'Unknown "bool")))))))

   (equal?
    (<PREDICATE>
     '("foo" "=" 42 "AND"
       "val" "NOT" "BETWEEN" 0.0 "AND" 1.0)
     Ok-only-if-empty
     (lambda () 'failure))
    `(success
      (,(predicate
         (comparison "=" (field 'Unknown "foo") 42)
         (list (partial-predicate
                "AND"
                (negation-predicate
                 (between-predicate (field 'Unknown "val") 0. 1.))))))))

   (equal?
    (<PREDICATE>
     '("val" "BETWEEN" 0.0 "AND" 1.0
       "AND" "foo" "=" 42)
     Ok-only-if-empty
     (lambda () 'failure))
    `(success
      (,(predicate
         (between-predicate (field 'Unknown "val") 0. 1.)
         (list (partial-predicate "AND" (comparison "=" (field 'Unknown "foo") 42)))))))

   (equal?
    (<ORDER-BY>
     '("ORDER" "BY" "Table_1" "." "abc")
     Ok-only-if-empty
     (lambda () 'failure))
    `(success
      (,(order-by "ASC" (list (field "Table_1" "abc"))))))

   (equal?
    (<ORDER-BY>
     '("ORDER" "BY" "Table_1" "." "abc" "," "fun" "(" "xyz" ")")
     Ok-only-if-empty
     (lambda () 'failure))
    `(success
      (,(order-by
         "ASC"
         (list (field "Table_1" "abc")
               (function-call "fun" (list (field 'Unknown "xyz"))))))))

   ;; TODO
   '(equal?
     (<SQL>
      "SELECT a FROM T WHERE ((X OR Y) AND (Z OR W)) ;"
      Ok-only-if-empty
      (lambda () 'failure))
     'TODO)    
   
   '(equal?
     (SQL->AST "SELECT * FROM T1 JOIN (SELECT * FROM T2) USING (id) ;"
               'foo))

   (sql->ast "SELECT *
FROM Object O, Source S
CROSSMATCH (O, ra_PS, decl_PS) 
       AND (S, ra, decl)
WITH RADIUS 0.0001
WHERE O.objectid = 1234 ;
")

   (ast->sql (sql->ast "select * from t where not a;"))

   (ast->sql (sql->ast "select * from t where not a between 1 AND 2;"))
   
   ;; Unit test end
   ))

