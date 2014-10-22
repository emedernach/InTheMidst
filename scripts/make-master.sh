#!/bin/bash

# Usage:
# /tmp/make-master.sh "psql -p 5280 postgres" object /tmp/object.template 4
# /tmp/make-master.sh "psql -p 5280 postgres" source /tmp/source.template 4

MASTER=$1
TABLE_NAME=$2
TABLE_TEMPLATE=$3
TOTAL_SHARDS=$4

function declare() {
    ${MASTER} <<EOF

CREATE EXTENSION postgres_fdw ;

-- For all pools

CREATE SERVER pool1
  FOREIGN DATA WRAPPER postgres_fdw
  OPTIONS (host 'localhost', port '5281', dbname 'postgres');

-- For all pools

CREATE USER MAPPING
  FOR manu SERVER pool1
  OPTIONS ( user 'manu', password '' );

EOF
}

function create_foreign_table() {
    POOL=pool$1

    I=$(($1 - 1))
    
    # Pad $1 with zeroes in PAD variable
    printf -v PAD "%03d" $I
    TABLE=${TABLE_NAME}_${PAD}

    
    ${MASTER} <<EOF
CREATE FOREIGN TABLE master_${TABLE} (
`cat ${TABLE_TEMPLATE}`
) SERVER ${POOL}
  OPTIONS ( table_name '${TABLE}' );
EOF
    }


for i in `seq ${TOTAL_SHARDS}`
do
    create_foreign_table $i
done