#!/bin/bash

# ./dataload.sh "zcat /data/databases/pt12.corrected/Object.csv.gz" 'psql -p 5280 -U postgres' object ~/src/LambdaInTheSky.dir/templates/object.template ra_PS decl_PS

# ./dataload.sh "zcat /data/databases/pt12.corrected/Source.csv.gz" 'psql -p 5280 -U postgres' source ~/src/LambdaInTheSky.dir/templates/source.template raobject declobject

DATAPROVIDER=$1
DBCLIENT=$2
TABLE=$3
TABLE_TEMPLATE=$4
RA=$5
DECL=$6

PIECE=`mktemp`
chmod a+r ${PIECE}

function split_and_run () {
# LMAX=1000000
LMAX=100000
I=1

${DATAPROVIDER} |  while read -r line
do
  I=$((I+1))
  echo "${line}" >> ${PIECE}
  if [ $I -gt ${LMAX} ]
  then
   master-loading.sh "${DBCLIENT}" "${TABLE}" "${TABLE_TEMPLATE}" "cat ${PIECE}" "${RA}" "${DECL}"
   I=1
   cat /dev/null > ${PIECE}
  fi
done

master-loading.sh "${DBCLIENT}" "${TABLE}" "${TABLE_TEMPLATE}" "cat ${PIECE}" "${RA}" "${DECL}"

rm -f ${PIECE}

}


split_and_run