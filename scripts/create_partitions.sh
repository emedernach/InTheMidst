#!/bin/bash

# Usage:
# ./create_partitions.sh 'psql -p 5280 postgres -U postgres' 'zcat /data/databases/pt12.corrected/Object.csv.gz' ra_PS decl_PS 0.05 /home/benchmark/src/LambdaInTheSky.dir/templates/object.template

DBCLIENT=$1
DATAPROVIDER=$2
RA=$3
DECL=$4
LENGTH=$5
TABLE_TEMPLATE=$6

function initialize () {
${DBCLIENT} <<EOF

DROP TABLE IF EXISTS partitions;
DROP TABLE IF EXISTS partitions_tmp;

CREATE TABLE partitions_tmp (
  x_min numeric NOT NULL,
  y_min numeric NOT NULL,
  z_min numeric NOT NULL
) ;

CREATE TABLE partitions (
  x_min numeric NOT NULL,
  y_min numeric NOT NULL,
  z_min numeric NOT NULL,
  UNIQUE (x_min, y_min, z_min)
) ;

DROP TABLE IF EXISTS tmp ;

CREATE TABLE tmp (
`cat ${TABLE_TEMPLATE}`
) ;

EOF
}

function read_and_create_pieces () {
PIECE=`mktemp`
LMAX=1000000
I=1

chmod a+r ${PIECE}

${DATAPROVIDER} |  while read -r line
do
  I=$((I+1))
  echo "${line}" >> ${PIECE}
  if [ $I -gt ${LMAX} ]
  then
   process_piece ${PIECE}
   I=1
   cat /dev/null > ${PIECE}
  fi
done

process_piece ${PIECE}
rm -f ${PIECE}
}

function process_piece () {
 PIECE=$1
 
 ${DBCLIENT} <<EOF
\timing

-- Data Loading
COPY tmp
FROM '${PIECE}'
WITH ( FORMAT csv, NULL 'NULL' ) ;
-- DELIMITER E'\t',  
-- NULL '\N') ;

-- Il faut definir une taille de partition:
\set len ${LENGTH}

INSERT INTO partitions_tmp
SELECT DISTINCT
  CAST(:len*floor(cos(radians(${RA}))*cos(radians(${DECL}))/:len) AS NUMERIC) as x_min,
  CAST(:len*floor(sin(radians(${RA}))*cos(radians(${DECL}))/:len) AS NUMERIC) as y_min,
  CAST(:len*floor(sin(radians(${DECL}))/:len) AS NUMERIC) as z_min
FROM tmp ;

DELETE FROM tmp ;

EOF
}

function finalize () {
${DBCLIENT} <<EOF
\timing

INSERT INTO partitions
SELECT DISTINCT x_min, y_min, z_min
FROM partitions_tmp 
ORDER BY x_min, y_min, z_min ;

ALTER TABLE partitions 
ADD COLUMN id BIGSERIAL PRIMARY KEY;

DROP TABLE partitions_tmp ;

DROP TABLE tmp ;

EOF
}


initialize
read_and_create_pieces
finalize 
