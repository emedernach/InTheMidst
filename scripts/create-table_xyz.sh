#!/bin/bash

# Usage:
# create-table_xyz.sh "psql -p 5281 postgres" Object /tmp/object.template 012

DBCLIENT=$1
TABLE_NAME=$2
TABLE_TEMPLATE=$3
SHARD=$4

TABLE=${TABLE_NAME}_${SHARD}_XYZ

${DBCLIENT} <<EOF
\timing

-- Table Creation

DROP TABLE IF EXISTS ${TABLE} ;

CREATE TABLE ${TABLE} (
`cat ${TABLE_TEMPLATE}`
) ;
EOF
