#!/bin/bash

# master.sh 'psql -p 5280 -U postgres' local_object src/LambdaInTheSky.dir/templates/object.template 'gunzip -c /data/databases/pt12.petasky/ObjectPetasky.csv.gz' ra_PS decl_PS

# In fact loading could be done by splitting the Object tables and
# loading each split separately

DBCLIENT=$1
TABLE=$2
TABLE_TEMPLATE=$3
DATAPROVIDER=$4
RA=$5
DECL=$6

function load_local() {
FIFO=/tmp/${TABLE}.fifo

rm -f ${FIFO}
mkfifo ${FIFO}

echo "${DATAPROVIDER} > ${FIFO} &"

${DATAPROVIDER} > ${FIFO} &

${DBCLIENT} <<EOF
\timing

-- Table Creation

DROP TABLE IF EXISTS local_${TABLE} ;

CREATE TABLE local_${TABLE} (
`cat ${TABLE_TEMPLATE}`
) ;

-- Data Loading
COPY local_${TABLE}
FROM '${FIFO}'
WITH ( FORMAT csv, NULL 'NULL' ) ;
-- DELIMITER E'\t',  
-- NULL '\N') ;

CREATE INDEX local_x on local_${TABLE} 
using btree ((cos(radians(${RA})) * cos(radians(${DECL})))) ;
CREATE INDEX local_y on local_${TABLE} 
using btree ((sin(radians(${RA})) * cos(radians(${DECL})))) ;
CREATE INDEX local_z on local_${TABLE} 
using btree ((sin(radians(${DECL})))) ;

EOF

wait
rm -f ${FIFO}
}

function populating () {
while read NUM XMIN YMIN ZMIN
do
 LEN=0.05
 XMAX=`echo $XMIN + ${LEN} | bc`    
 YMAX=`echo $YMIN + ${LEN} | bc`    
 ZMAX=`echo $ZMIN + ${LEN} | bc`

 SHARD=master_${TABLE}_${NUM}_xyz

 echo
 echo Populating ${SHARD} ${XMIN} ${XMAX} ${YMIN} ${YMAX} ${ZMIN} ${ZMAX}

${DBCLIENT} <<EOF
\timing

-- Populating ${SHARD}

INSERT INTO ${SHARD}
SELECT * FROM local_${TABLE} 
WHERE cos(radians(${RA})) * cos(radians(${DECL})) BETWEEN ${XMIN} AND ${XMAX}
  AND sin(radians(${RA})) * cos(radians(${DECL})) BETWEEN ${YMIN} AND ${YMAX}
  AND sin(radians(${DECL})) BETWEEN ${ZMIN} AND ${ZMAX} ;

EOF
 
done <<EOF
001  0.95  -0.05  -0.15
002  0.95  -0.05   -0.1
003  0.95  -0.05  -0.05
004  0.95  -0.05      0
005  0.95  -0.05   0.05
006  0.95  -0.05    0.1
007  0.95      0  -0.15
008  0.95      0   -0.1
009  0.95      0  -0.05
010  0.95      0      0
011  0.95      0   0.05
012  0.95      0    0.1
013  0.95   0.05  -0.15
014  0.95   0.05   -0.1
015  0.95   0.05  -0.05
016  0.95   0.05      0
017  0.95   0.05   0.05
018  0.95   0.05    0.1
EOF

${DBCLIENT} <<EOF
\timing

DROP TABLE local_${TABLE} ;

EOF

}


echo Loading data into local tables
load_local
echo Done

echo Populating foreign tables
populating
echo Done