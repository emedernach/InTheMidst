#!/bin/bash

POOL1=193.48.81.97
POOL2=193.48.81.98
POOL3=193.48.81.99
POOL4=193.48.81.100

# -- Master --

COMMAND="psql -p 5280 postgres -U postgres"

(
${COMMAND} <<EOF 
\d
EOF
)  | grep foreign_cache__ | awk '//{ print $3; }' | while read FTABLE
do
echo Dropping foreign table ${FTABLE}
${COMMAND} <<EOF 
DROP FOREIGN TABLE ${FTABLE} ;
EOF
done 

# -- Pools --

while read IP
do
COMMAND="psql -p 5280 -h ${IP} -U postgres"

(
${COMMAND} <<EOF 
\d
EOF
)  | grep cache__ | awk '//{ print $3; }' | while read CTABLE
do

echo Dropping cache table ${CTABLE}
${COMMAND} <<EOF 
DROP TABLE ${CTABLE} ;
EOF
done 

done <<EOF
${POOL1}
${POOL2}
${POOL3}
${POOL4}
EOF


