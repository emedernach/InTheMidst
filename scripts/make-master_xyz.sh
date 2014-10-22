#!/bin/bash

# Usage:
# /tmp/make-master_xyz.sh  object /tmp/object.template 
# /tmp/make-master_xyz.sh  source /tmp/source.template 

MASTER=5280
TABLE_NAME=$1
TABLE_TEMPLATE=$2

COMMAND="psql -p 5280 -U postgres"

function create_foreign_table() {
    PORT=$1
    POOL=$2
    SHARD=$3
    
    TABLE=${TABLE_NAME}_${SHARD}_xyz
    echo "== CREATING FOREIGN TABLE FROM ${TABLE} ON POOL ${POOL}=="
    
    ${COMMAND} <<EOF
DROP FOREIGN TABLE IF EXISTS master_${TABLE} ;
CREATE FOREIGN TABLE master_${TABLE} (
`cat ${TABLE_TEMPLATE}`
) SERVER ${POOL}
  OPTIONS ( table_name '${TABLE}' );
EOF
}



POOL1=pool1
POOL2=pool2
POOL3=pool3
POOL4=pool4

while read PORT POOL SHARD
do
    create_foreign_table ${PORT} ${POOL} ${SHARD}
done << EOF
   ${POOL3}    pool3   001
   ${POOL1}    pool1   002
   ${POOL1}    pool1   003
   ${POOL4}    pool4   004
   ${POOL2}    pool2   005
   ${POOL3}    pool3   006
   ${POOL2}    pool2   007
   ${POOL4}    pool4   008
   ${POOL1}    pool1   009
   ${POOL2}    pool2   010
   ${POOL3}    pool3   011
   ${POOL3}    pool3   012
   ${POOL3}    pool3   013
   ${POOL1}    pool1   014
   ${POOL3}    pool3   015
   ${POOL4}    pool4   016
   ${POOL2}    pool2   017
   ${POOL3}    pool3   018
EOF
