
# Copyright (C) 2013-2014 Emmanuel Medernach
#
# This file is part of "In The Midst".
#
# "In The Midst" is  free software: you can redistribute it
# and/or  modify  it under  the  terms  of  the GNU  Lesser
# General Public License as  published by the Free Software
# Foundation, either version 3  of the License, or (at your
# option) any later version.
# 
# "In The Midst" is distributed in the hope that it will be
# useful,  but  WITHOUT  ANY  WARRANTY;  without  even  the
# implied  warranty  of MERCHANTABILITY  or  FITNESS FOR  A
# PARTICULAR  PURPOSE.  See the  GNU Lesser  General Public
# License for more details.
# 
# You should have received a  copy of the GNU Lesser General
# Public  License along with  "In The  Midst".  If  not, see
# <http://www.gnu.org/licenses/>.
#
## Author: Emmanuel Medernach

# Configuration

GAMBITDIR = /opt/scheme/gambit/4.6.9
POSTGRESDIR = /opt/postgresql/9.3.5

# ----

GSC = $(GAMBITDIR)/bin/gsc
GAMBCINCLUDE = $(GAMBITDIR)/include/
LIBGAMBC = $(GAMBITDIR)/lib/
SYNTAXCASE = $(GAMBITDIR)/lib/syntax-case.scm

PQINCLUDE = $(POSTGRESDIR)/include/
LIBPQ = $(POSTGRESDIR)/lib/

CC = gcc 
CCOPTS =  -I$(PQINCLUDE) -I$(GAMBCINCLUDE) -L$(LIBGAMBC) -L$(LIBPQ)
CCOPTS += -lm -ldl -lgambc -lutil -lpq
CCOPTS += -Wall

# ----

CFILES =  obj/c/list.c obj/c/string.c obj/c/set.c
CFILES += obj/c/bnf.c obj/c/parser.c obj/c/transform.c
CFILES += obj/c/sql_reader.c obj/c/sql_transformer.c
CFILES += obj/c/sql_compiler.c obj/c/schema.c obj/c/table-operations.c
CFILES += obj/c/ast_to_sql.c obj/c/collect.c
CFILES += obj/c/replace_table.c obj/c/replace_function.c obj/c/count.c
CFILES += obj/c/sql_functions.c obj/c/qserv.c 
CFILES += obj/c/expand_or.c obj/c/rewriter.c
CFILES += obj/c/stringset.c obj/c/insert_predicate.c
CFILES += obj/c/distribute_predicates.c obj/c/collect_fields.c
CFILES += obj/c/partition-rewrite.c obj/c/join.c 
CFILES += obj/c/spatialjoin.c obj/c/syntaxcase.c
CFILES += obj/c/fusion.c obj/c/innerjoin.c obj/c/validate.c
CFILES += obj/c/LSST_queries.c obj/c/SDSS_queries.c
CFILES += obj/c/description_xyz.c obj/c/subquery.c
CFILES += obj/c/ticketqueue.c obj/c/tabletypes.c
CFILES += obj/c/samepool.c obj/c/frame.c
CFILES += obj/c/placeholder.c obj/c/scope.c
CFILES += obj/c/describe.c obj/c/from.c
CFILES += obj/c/freevar.c obj/c/externalquery.c
CFILES += obj/c/replacetable.c obj/c/cachetable.c
CFILES += obj/c/deanonymize.c obj/c/plan.c
CFILES += obj/c/type.c obj/c/query.c
CFILES += obj/c/parallel.c obj/c/execute.c
CFILES += obj/c/libpq.c obj/c/postgresql.c
CFILES += obj/c/test.c obj/c/main.c

#  obj/c/Petasky_queries.c

all: $(CFILES) obj/o/link.o obj/o/pq.o
	$(CC) -o itm obj/o/*.o $(CCOPTS)
	strip itm

clean:
	rm -f itm
	rm -f obj/c/*.c
	rm -f obj/o/*.o

obj/o/pq.o: src/database.dir/pq.c
	$(CC) -o obj/o/pq.o -I$(PQINCLUDE) -c src/database.dir/pq.c

obj/o/link.o: obj/c/link.c $(CFILES)
	$(GSC) -o obj/o/ -cc-options "-I$(PQINCLUDE)" -obj $(CFILES) obj/c/link.c 

obj/c/link.c: $(CFILES)
	$(GSC) -o $@ -link $(CFILES)

## C Files

obj/c/main.c: src/main.scm
#	$(GSC) -:s -o $@ -c $^
	$(GSC) -o $@ -c $^

obj/c/list.c: src/utils.dir/list.scm
#	$(GSC) -:s -o $@ -c $^
	$(GSC) -o $@ -c $^

obj/c/string.c: src/utils.dir/string.scm
	$(GSC) -:s -o $@ -c $^

obj/c/set.c: src/utils.dir/set.scm
	$(GSC) -:s -o $@ -c $^

obj/c/queue.c: src/utils.dir/queue.scm
	$(GSC) -:s -o $@ -c $^

obj/c/stringset.c: src/utils.dir/stringset.scm
	$(GSC) -:s -o $@ -c $^

obj/c/subquery.c: src/ast.dir/subquery.scm
	$(GSC) -:s -o $@ -c $^

obj/c/bnf.c: src/parser.dir/bnf.scm
	$(GSC) -:s -o $@ -c $^

obj/c/parser.c: src/parser.dir/parser.scm
	$(GSC) -:s -o $@ -c $^

obj/c/transform.c: src/parser.dir/transform.scm
	$(GSC) -:s -o $@ -c $^

obj/c/table-operations.c: src/sql.dir/table-operations.scm
	$(GSC) -:s -o $@ -c $^

obj/c/sql_reader.c: src/sql.dir/sql_reader.scm
	$(GSC) -:s -o $@ -c $^

obj/c/sql_transformer.c: src/sql.dir/sql_transformer.scm
	$(GSC) -:s -o $@ -c $^

obj/c/sql_compiler.c: src/sql.dir/sql_compiler.scm
	$(GSC) -:s -o $@ -c $^

obj/c/schema.c: src/sql.dir/schema.scm
	$(GSC) -:s -o $@ -c $^

obj/c/ast_to_sql.c: src/rewriter.dir/ast_to_sql.scm
	$(GSC) -:s -o $@ -c $^

obj/c/collect.c: src/rewriter.dir/collect.scm
	$(GSC) -:s -o $@ -c $^

obj/c/insert_predicate.c: src/rewriter.dir/insert_predicate.scm
	$(GSC) -:s -o $@ -c $^ 

obj/c/distribute_predicates.c: src/rewriter.dir/distribute_predicates.scm
	$(GSC) -:s -o $@ -c $^ 

obj/c/collect_fields.c: src/rewriter.dir/collect_fields.scm
	$(GSC) -:s -o $@ -c $^

obj/c/validate.c: src/rewriter.dir/validate.scm
	$(GSC) -:s -o $@ -c $^

# obj/c/disambiguation.c: src/rewriter.dir/disambiguation.scm
# 	$(GSC) -:s -o $@ -c $^

obj/c/replace_table.c: src/rewriter.dir/replace_table.scm
	$(GSC) -:s -o $@ -c $^

obj/c/sql_functions.c: src/rewriter.dir/sql_functions.scm
	$(GSC) -:s -o $@ -c $^

obj/c/replace_function.c: src/rewriter.dir/replace_function.scm
	$(GSC) -:s -o $@ -c $^

obj/c/partition-rewrite.c: src/rewriter.dir/partition-rewrite.scm
	$(GSC) -:s -o $@ -c $^

obj/c/join.c: src/rewriter.dir/join.scm
	$(GSC) -:s -o $@ -c $^

obj/c/geometry.c: src/rewriter.dir/geometry.scm
	$(GSC) -:s -o $@ -c $^

obj/c/spatialjoin.c: src/rewriter.dir/spatialjoin.scm
	$(GSC) -:s -o $@ -c $^

obj/c/innerjoin.c: src/rewriter.dir/innerjoin.scm
	$(GSC) -:s -o $@ -c $^

obj/c/fusion.c: src/rewriter.dir/fusion.scm
	$(GSC) -:s -o $@ -c $^

obj/c/expand_or.c: src/rewriter.dir/expand_or.scm
	$(GSC) -:s -o $@ -c $^

obj/c/count.c: src/rewriter.dir/count.scm
	$(GSC) -:s -o $@ -c $^

obj/c/rewriter.c: src/rewriter.dir/rewriter.scm
	$(GSC) -:s -o $@ -c $^

obj/c/deanonymize.c: src/rewriter.dir/deanonymize.scm
	$(GSC) -:s -o $@ -c $^

obj/c/description_xyz.c: src/examples.dir/description_xyz.scm
	$(GSC) -:s -o $@ -c $^

obj/c/LSST_queries.c: src/examples.dir/LSST_queries.scm
	$(GSC) -:s -o $@ -c $^

# obj/c/Petasky_queries.c: src/examples.dir/Petasky_queries.scm
#	$(GSC) -:s -o $@ -c $^

obj/c/qserv.c: src/examples.dir/qserv.scm
	$(GSC) -:s -o $@ -c $^

obj/c/SDSS_queries.c: src/examples.dir/SDSS_queries.scm
	$(GSC) -:s -o $@ -c $^

obj/c/test.c: src/test.scm
	$(GSC) -:s -o $@ -c $^

obj/c/libpq.c: src/database.dir/libpq.scm
	$(GSC) -:s -o $@ -c $^

obj/c/postgresql.c: src/database.dir/postgresql.scm
	$(GSC) -:s -o $@ -c $^

obj/c/execute.c: src/execution.dir/execute.scm
	$(GSC) -:s -o $@ -c $^

obj/c/parallel.c: src/execution.dir/parallel.scm
	$(GSC) -:s -o $@ -c $^

obj/c/ticketqueue.c: src/execution.dir/ticketqueue.scm
	$(GSC) -:s -o $@ -c $^

obj/c/tabletypes.c: src/execution.dir/tabletypes.scm
	$(GSC) -:s -o $@ -c $^

obj/c/samepool.c: src/execution.dir/samepool.scm
	$(GSC) -:s -o $@ -c $^

obj/c/frame.c: src/execution.dir/frame.scm
	$(GSC) -:s -o $@ -c $^

obj/c/placeholder.c: src/execution.dir/placeholder.scm
	$(GSC) -:s -o $@ -c $^

obj/c/scope.c: src/execution.dir/scope.scm
	$(GSC) -:s -o $@ -c $^

obj/c/describe.c: src/execution.dir/describe.scm
	$(GSC) -:s -o $@ -c $^

obj/c/from.c: src/execution.dir/from.scm
	$(GSC) -:s -o $@ -c $^

obj/c/freevar.c: src/execution.dir/freevar.scm
	$(GSC) -:s -o $@ -c $^

obj/c/externalquery.c: src/execution.dir/externalquery.scm
	$(GSC) -:s -o $@ -c $^

obj/c/replacetable.c: src/execution.dir/replacetable.scm
	$(GSC) -:s -o $@ -c $^

obj/c/cachetable.c: src/execution.dir/cachetable.scm
	$(GSC) -:s -o $@ -c $^

obj/c/type.c: src/execution.dir/type.scm
	$(GSC) -:s -o $@ -c $^

obj/c/query.c: src/execution.dir/query.scm
	$(GSC) -:s -o $@ -c $^

obj/c/plan.c: src/execution.dir/plan.scm
	$(GSC) -:s -o $@ -c $^

obj/c/syntaxcase.c: $(SYNTAXCASE)
	$(GSC) -:s -o $@ -c $^