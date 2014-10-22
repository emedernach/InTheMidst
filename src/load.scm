
;; With Gambit Scheme:
;; gsi -:s

;; (current-directory ".. Lattices/Distributivity/src")

(define (verbose-load file)
  (display " -- Loading ")
  (display file)
  (load file)
  (display "  [Done]")
  (newline))

;; We could add a timer here if needed. 
(define (loaded) "Ok")

;; Gambit and Guile for the moment
(verbose-load "compatibility.scm")


;; SRFI extensions
;; (display "Loading SRFI extensions  ")
;; (verbose-load "extensions.dir/case-lambda.scm")
;; (display (loaded)) (newline)

;; Macros + DSL
(newline)
(display "Macros + DSL  ") (newline)
(verbose-load "macros.dir/macros.scm")
(display (loaded)) (newline)

;; Utilities
(newline)
(display "Loading Utilities  ") (newline)
(verbose-load "utils.dir/list.scm")
;; (verbose-load "utils.dir/queue.scm")
(verbose-load "utils.dir/string.scm")
(verbose-load "utils.dir/set.scm")
(verbose-load "utils.dir/stringset.scm")
(display (loaded)) (newline)

;; Structures + AST
(newline)
(display "Loading AST structures  ") (newline)
(verbose-load "ast.dir/token.scm")
(verbose-load "ast.dir/sql.scm")
(verbose-load "ast.dir/table.scm")
(verbose-load "ast.dir/partition.scm")
(verbose-load "ast.dir/modifier.scm")
(verbose-load "ast.dir/function.scm")
(verbose-load "ast.dir/projection.scm")
(verbose-load "ast.dir/predicate.scm")
(verbose-load "ast.dir/logic.scm")
(verbose-load "ast.dir/arithmetic.scm")
(verbose-load "ast.dir/subquery.scm")
(display (loaded)) (newline)

;; Lexer, parser and compiler
(newline)
(display "Loading Generic Lexer, Parser and Compiler  ")  (newline)
(verbose-load "parser.dir/bnf.scm")
(verbose-load "parser.dir/parser.scm")
(verbose-load "parser.dir/transform.scm")
(display (loaded)) (newline)

;; SQL related
(newline)
(display "Loading SQL Parser and Compiler  ")  (newline)
(verbose-load "sql.dir/table-operations.scm")
(verbose-load "sql.dir/sql_reader.scm")
;; (verbose-load "sql.dir/sql_parser.scm")
(verbose-load "sql.dir/sql_transformer.scm")
(verbose-load "sql.dir/sql_compiler.scm")
(verbose-load "sql.dir/schema.scm")
(display (loaded)) (newline)

;; AST related
(newline)
(display "Loading AST rewriter  ")  (newline)
(verbose-load "rewriter.dir/ast_to_sql.scm")
(verbose-load "rewriter.dir/collect.scm")
(verbose-load "rewriter.dir/validate.scm")
(verbose-load "rewriter.dir/sql_functions.scm")
(verbose-load "rewriter.dir/replace_table.scm")
(verbose-load "rewriter.dir/replace_function.scm")
(verbose-load "rewriter.dir/collect_fields.scm")
(verbose-load "rewriter.dir/insert_predicate.scm")
(verbose-load "rewriter.dir/distribute_predicates.scm")
(verbose-load "rewriter.dir/expand_or.scm")
(verbose-load "rewriter.dir/count.scm")
(verbose-load "rewriter.dir/join.scm")
(verbose-load "rewriter.dir/innerjoin.scm")
(verbose-load "rewriter.dir/fusion.scm")
(verbose-load "rewriter.dir/spatialjoin.scm")
(verbose-load "rewriter.dir/rewriter.scm")
(verbose-load "rewriter.dir/partition-rewrite.scm")
(verbose-load "rewriter.dir/deanonymize.scm")
(display (loaded)) (newline)


(newline)
(display "Loading Query executor   ")  (newline)
(verbose-load "execution.dir/ticketqueue.scm")
(verbose-load "execution.dir/tabletypes.scm")
(verbose-load "execution.dir/samepool.scm")
(verbose-load "execution.dir/frame.scm")
(verbose-load "execution.dir/placeholder.scm")
(verbose-load "execution.dir/scope.scm")
(verbose-load "execution.dir/describe.scm")
(verbose-load "execution.dir/from.scm")
(verbose-load "execution.dir/freevar.scm")
(verbose-load "execution.dir/externalquery.scm")
(verbose-load "execution.dir/replacetable.scm")
(verbose-load "execution.dir/cachetable.scm")
(verbose-load "execution.dir/type.scm")
(verbose-load "execution.dir/query.scm")
(verbose-load "execution.dir/plan.scm")
(display (loaded)) (newline)


;; Examples
(newline)
(display "Loading Examples.  ") (newline)
(verbose-load "examples.dir/description_xyz.scm")
(verbose-load "examples.dir/LSST_queries.scm")
(verbose-load "examples.dir/Petasky_queries.scm")
(verbose-load "examples.dir/qserv.scm")
(verbose-load "test.scm")
(display (loaded)) (newline)

(newline)
(display "Finished.  ")
(display (loaded)) (newline)
(newline)

(display "You could try examples queries with :") (newline)
;; (display "  (LSST_queries:example)") (newline)
(display "  (LSST_queries:example-without-printing)") (newline)
(display "  (QSERV_queries:example-without-printing)") (newline)
(display "or enter SQL string queries interactively with :") (newline)
(display "  (interactive)") (newline)
(display "or run test suite with :") (newline)
(display "  (test-all)") (newline)
(newline)
         
;; ----
;;;;  
;;;;  ;; (verbose-load "data-structures.dir/select.scm")
;;;;  (verbose-load "sql.scm")
;;;;  (verbose-load "partial-eval.scm")
;;;;  
;;;;  
;;;;  (verbose-load "case-lambda.scm")
;;;;  
;;;;  
;;;;  (verbose-load "table.scm")
;;;;  (verbose-load "set.scm")
;;;;  ;; (verbose-load "rewrite.scm") ;; not needed !
;;;;  
;;;;  (verbose-load "predicate.scm")
;;;;  (verbose-load "columns.scm")
;;;;  
;;;;  
;;;;  ;; (verbose-load "parser.dir/tmp.scm")
;;;;  
;;;;  ;; TODO: todo.scm
;;;;  
;;;;  ;; ----

