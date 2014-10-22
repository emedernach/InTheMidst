#include <stdio.h>
#include <libpq-fe.h>

int pqstatus_wrapper(const PGconn *conn) {
  ConnStatusType status;

  if (conn == NULL) return 1;
  
  status = PQstatus(conn);
  switch(status) {
  case CONNECTION_OK:
    return 0;
  default:
    return 1;
  }
}

/**
   Do we have to expose the "PGresult *" type to users ?
   It is needed by libpq to retrieve all query results but ...
 **/

// For SELECT queries
PGresult *sql_select(PGconn *conn,
                     char *select_query) {
  PGresult *res = NULL;

  res = PQexec(conn, select_query);

  if ((res == NULL) ||
      (PQresultStatus(res) != PGRES_TUPLES_OK)) {
    PQclear(res);
    fprintf(stderr,
            "SQL SELECT Query failed: %s\n",
            PQerrorMessage(conn));
    
    // Converted to #f in Gambit Scheme
    return NULL; 
  }

  return res;
}

// For CREATE TABLE or other command queries
int sql_command(PGconn *conn,
                char *select_query) {
  PGresult *res = NULL;

  res = PQexec(conn, select_query);

  if ((res == NULL) ||
      (PQresultStatus(res) != PGRES_COMMAND_OK)) {
    PQclear(res);
    fprintf(stderr,
            "SQL Command failed: %s\n",
            PQerrorMessage(conn));
    return 1; 
  }

  PQclear(res);
  return 0;
}
    
