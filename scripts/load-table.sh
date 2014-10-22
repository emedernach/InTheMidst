#!/bin/bash

# Usage:
# load-table.sh "psql -p 5280 postgres" Filter /tmp/filter.template "gunzip -c /data/clrlsstwn04/database/pt12/Filter.csv.gz"

CLIENT=$1
TABLE=$2
TABLE_TEMPLATE=$3
READ_DATA=$4

FIFO=/tmp/${TABLE}.fifo


${CLIENT} <<EOF
\timing

-- Table Creation

DROP TABLE IF EXISTS ${TABLE} ;

CREATE TABLE ${TABLE} (
`cat ${TABLE_TEMPLATE}`
) ;
EOF

echo
echo == Data Loading ==
echo

rm -f ${FIFO}
mkfifo ${FIFO}

${READ_DATA} > ${FIFO} &

${CLIENT} <<EOF
\timing

-- Data Loading
COPY ${TABLE}
FROM '${FIFO}'
DELIMITERS ','
WITH NULL 'NULL'
CSV QUOTE '''' ;

EOF


wait
rm -f ${FIFO}