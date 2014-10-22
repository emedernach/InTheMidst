#!/bin/bash
# set -x

# Data loading for PostgreSQL

# XYZ sharding (same length)

# Usage:

# make-xyz-shard.sh
#   dbclient table template idshard
#   dataprovider
#   raColumn declColumn
#   xmin xmax ymin ymax zmin zmax 

# About raColumn declColumn (PT1.2) :
#   Object:  ra_PS = Colonne 3  /  decl_PS = Colonne 5
#   Source:  raObject = Colonne 34 / declObject = Colonne 35

# Example:
#
# make-xyz-shard.sh 
#  "psql -p 5281 postgres" Object /tmp/object.template 012
#  "gunzip -c /data/databases/pt12.petasky/ObjectPetasky.csv.gz"
#  3 5 
#  0.95 1.0 0.05 0.1 -0.1 -0.05

# make-xyz-shard.sh "psql -p 5281 postgres" Object /tmp/object.template 012 "gunzip -c /data/databases/pt12.petasky/ObjectPetasky.csv.gz" 3 5 0.95 1.0 0.05 0.1 -0.1 -0.05

DBCLIENT=$1
TABLE_NAME=$2
TABLE_TEMPLATE=$3
SHARD=$4
DATAPROVIDER=$5
RA=$6
DECL=$7
XMIN=$8
XMAX=$9
YMIN=${10}
YMAX=${11}
ZMIN=${12}
ZMAX=${13}

echo
echo  DBCLIENT=$DBCLIENT
echo  TABLE_NAME=$TABLE_NAME
echo  TABLE_TEMPLATE=$TABLE_TEMPLATE
echo  SHARD=$SHARD
echo  DATAPROVIDER=$DATAPROVIDER
echo  RA=$RA
echo  DECL=$DECL
echo  XMIN=$XMIN
echo  XMAX=$XMAX
echo  YMIN=$YMIN
echo  YMAX=$YMAX
echo  ZMIN=$ZMIN
echo  ZMAX=$ZMAX
echo

TABLE=${TABLE_NAME}_${SHARD}_XYZ
FIFO=/tmp/${TABLE}.fifo

# TODO: we could add a CHECK constraint to the table creation in order
# to force ra/decl => x,y,z values to be inside the box

echo
echo == Data Loading ==
echo

rm -f ${FIFO}
mkfifo ${FIFO}

# -F',' if CSV

${DATAPROVIDER} | awk -F',' "
BEGIN { pi = atan2(0, -1); degrees = pi / 180; }
//{
ra = \$${RA} * degrees;
decl = \$${DECL} * degrees;

 x = cos(ra)*cos(decl) ;
 y = sin(ra)*cos(decl) ;
 z = sin(decl) ;
 
 if ((${XMIN} <= x) && (x < ${XMAX}))
  if ((${YMIN} <= y) && (y < ${YMAX}))
   if ((${ZMIN} <= z) && (z < ${ZMAX}))
    print ;
 }" > ${FIFO} &


${DBCLIENT} <<EOF
\timing

-- Data Loading
COPY local_${TABLE}
FROM '${FIFO}'
DELIMITERS ','
WITH NULL 'NULL' 
CSV  QUOTE '''' ;

INSERT INTO master_${TABLE}
SELECT * FROM local_${TABLE} ;

DROP TABLE local_${TABLE} ;

EOF

wait
rm -f ${FIFO}

echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
echo "!!                                             !!"
echo "!!         Please run index_xyz.sh             !!"
echo "!!                                             !!"
echo "!! And recluster periodically if data is added !!"
echo "!!                                             !!"
echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

# Et pourquoi  ne pas faire le  decoupage en base ?  (Je suis serieux:
# les donnees arrivent par blocs de taille raisonnables)

##  Avec la version 9.3  l'insertion externe marche :
##  postgres=# CREATE FOREIGN TABLE testinsert (
##  postgres(# i integer,
##  postgres(# j integer,
##  postgres(# k integer
##  postgres(# ) SERVER pool1
##  postgres-# OPTIONS ( table_name 'tableof3ints' );
##  CREATE FOREIGN TABLE
##  Time: 190.192 ms
##  postgres=# select * from testinsert ;
##   i | j | k 
##  ---+---+---
##  (0 rows)
##  
##  Time: 1.005 ms
##  postgres=# insert into testinsert (i,j,k) values (3,4,5) ;
##  INSERT 0 1
##  Time: 18.500 ms
##  postgres=# select * from testinsert ;
##   i | j | k 
##  ---+---+---
##   3 | 4 | 5
##  (1 row)
##  


# Verification
#
# shuf -n 1 /tmp/a.result | awk '
# BEGIN { pi = atan2(0, -1); degrees = pi / 180; }
# // {
#  ra = $3 * degrees ;
#  decl = $5 * degrees ;
#  x = cos(ra)*cos(decl) ;
#  y = sin(ra)*cos(decl) ;
#  z = sin(decl) ;
#  print $1,$3,$5,ra,decl,x,y,z ;
# }'

# For more data (benchmarking):
# "gunzip -c /data/databases/pt12.petasky/ObjectPetasky.csv.gz"

# time /tmp/make-xyz-shard.sh "psql -p 5281 postgres" Object /tmp/object.template 014 "bunzip2 -c /home/postgres/databases/pt12/Object.csv.bz2" 3 5 0.95 1.0 0.05 0.1 -0.1 -0.05 | tee /tmp/a.result


# ------------------------------------------------------------ #


# TODO: We have to add keys and indexes after loading
# ALTER TABLE ${TABLE} ADD PRIMARY KEY();
# But the key is not necessarily ${SHARD_KEY} !

# Object shards:
# ALTER TABLE object_000 ADD PRIMARY KEY(objectId);

# Source shards:
# ALTER TABLE source_000 ADD PRIMARY KEY(sourceId);
# CREATE INDEX CONCURRENTLY source_object_idx ON source_000(objectId);

# On each shard we will load only the needed part
# for that we need to filter the data :

# For high precision in awk:
# awk -v 'CONVFMT=%.17g' -F',' '//{ printf("%0.17g \n", 1 + $3); }' z.csv 
# 1.12345678901234568 




