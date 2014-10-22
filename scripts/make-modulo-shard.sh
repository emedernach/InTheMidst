#!/bin/bash

# Data loading for PostgreSQL

# Usage:
# make-shard.sh "psql -p 5281 postgres" Source /tmp/source.template objectId 000 4 "gunzip -c /data/clrlsstwn04/database/pt12/Source.csv.gz" 4
# make-shard.sh "psql -p 5282 postgres" Source /tmp/source.template objectId 001 4 "gunzip -c /data/clrlsstwn04/database/pt12/Source.csv.gz" 4
# make-shard.sh "psql -p 5283 postgres" Source /tmp/source.template objectId 002 4 "gunzip -c /data/clrlsstwn04/database/pt12/Source.csv.gz" 4
# make-shard.sh "psql -p 5284 postgres" Source /tmp/source.template objectId 003 4 "gunzip -c /data/clrlsstwn04/database/pt12/Source.csv.gz" 4

# Templates are in the template directory

CLIENT=$1
TABLE_NAME=$2
TABLE_TEMPLATE=$3
SHARD_KEY=$4
SHARD=$5
TOTAL_SHARDS=$6
READ_DATA=$7
DATA_KEY=$8


TABLE=${TABLE_NAME}_${SHARD}
FIFO=/tmp/${TABLE}.fifo

${CLIENT} <<EOF
\timing

-- Table Creation

DROP TABLE IF EXISTS ${TABLE} ;

CREATE TABLE ${TABLE} (
`cat ${TABLE_TEMPLATE}`
,
 CHECK (${SHARD_KEY} % ${TOTAL_SHARDS} = ${SHARD})
) ;
EOF

# TODO: We have to add keys and indexes after loading
# ALTER TABLE ${TABLE} ADD PRIMARY KEY();
# But the key is not necessarily ${SHARD_KEY} !

# Object shards:
# ALTER TABLE object_000 ADD PRIMARY KEY(objectId);

# Source shards:
# ALTER TABLE source_000 ADD PRIMARY KEY(sourceId);
# CREATE INDEX CONCURRENTLY source_object_idx ON source_000(objectId);


echo
echo == Data Loading ==
echo

rm -f ${FIFO}
mkfifo ${FIFO}

# On each shard we will load only the needed part
# for that we need to filter the data :

${READ_DATA} | awk -F',' "//{ if (\$${DATA_KEY} % ${TOTAL_SHARDS} == ${SHARD}) print; }" > ${FIFO} &

${CLIENT} <<EOF
\timing

-- Data Loading
COPY ${TABLE}
FROM '${FIFO}'
DELIMITERS ','
WITH NULL 'NULL' 
CSV  QUOTE '''' ;

EOF

wait
rm -f ${FIFO}
