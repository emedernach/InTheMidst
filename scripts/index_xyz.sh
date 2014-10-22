#!/bin/bash

# TODO: il manque ANALYZE !

# object_004_xyz
# source_004_xyz

# L'avantage du decoupage en XYZ  c'est qu'on peut redecouper un chunk
# alors qu'avec ra,dec ca demande a redecouper l'ensemble.

POOL1=5281
POOL2=5282
POOL3=5283
POOL4=5284

while read PORT CHUNK
do

TABLE=object_${CHUNK}_xyz

echo == INDEXING ${TABLE} on port ${PORT} ==
    

psql -p ${PORT} postgres << EOF

CREATE INDEX ${TABLE}_idx_id ON ${TABLE} USING btree(objectid);

CREATE INDEX ${TABLE}_idx_xyz ON ${TABLE}
USING btree ((cos(radians(ra_PS))*cos(radians(decl_PS))),
             (sin(radians(ra_PS))*cos(radians(decl_PS))),
             (sin(radians(decl_PS))));

CREATE INDEX ${TABLE}_idx_ra on ${TABLE} using btree (ra_PS) ;
CREATE INDEX ${TABLE}_idx_decl on ${TABLE} using btree (decl_PS) ;  

CREATE INDEX ${TABLE}_idx_X on ${TABLE} using btree ((cos(radians(ra_PS))*cos(radians(decl_PS)))) ;
CREATE INDEX ${TABLE}_idx_Y on ${TABLE} using btree ((sin(radians(ra_PS))*cos(radians(decl_PS)))) ;  
CREATE INDEX ${TABLE}_idx_Z on ${TABLE} using btree ((sin(radians(decl_PS)))) ;  

CLUSTER ${TABLE} USING ${TABLE}_idx_xyz ;

EOF

TABLE=source_${CHUNK}_xyz

echo == INDEXING ${TABLE} on port ${PORT} ==

# NOTE: we could also index raObject,declObject and raFlux,raPeak (if necessary)

psql -p ${PORT} postgres << EOF

CREATE INDEX ${TABLE}_idx_id ON ${TABLE} USING btree(objectid);

CREATE INDEX ${TABLE}_idx_xyz ON ${TABLE}
USING btree ((cos(radians(ra))*cos(radians(decl))),
             (sin(radians(ra))*cos(radians(decl))),
             (sin(radians(decl))));

CREATE INDEX ${TABLE}_idx_ra on ${TABLE} using btree (ra) ;
CREATE INDEX ${TABLE}_idx_decl on ${TABLE} using btree (decl) ;  

CREATE INDEX ${TABLE}_idx_X on ${TABLE} using btree ((cos(radians(ra))*cos(radians(decl)))) ;
CREATE INDEX ${TABLE}_idx_Y on ${TABLE} using btree ((sin(radians(ra))*cos(radians(decl)))) ;  
CREATE INDEX ${TABLE}_idx_Z on ${TABLE} using btree ((sin(radians(decl)))) ;  

CLUSTER ${TABLE} USING ${TABLE}_idx_xyz ;

EOF

done << EOF
   ${POOL3}   001
   ${POOL1}   002
   ${POOL1}   003
   ${POOL4}   004
   ${POOL2}   005
   ${POOL3}   006
   ${POOL2}   007
   ${POOL4}   008
   ${POOL1}   009
   ${POOL2}   010
   ${POOL3}   011
   ${POOL3}   012
   ${POOL3}   013
   ${POOL1}   014
   ${POOL3}   015
   ${POOL4}   016
   ${POOL2}   017
   ${POOL3}   018
EOF

while read PORT CHUNK
do

TABLE=object_${CHUNK}_xyz

echo == INDEXING Photometry for ${TABLE} on port ${PORT} ==

psql -p ${PORT} postgres << EOF

CREATE INDEX ${TABLE}_idx_photo_u ON ${TABLE} USING btree(uFlux_PS);
CREATE INDEX ${TABLE}_idx_photo_g ON ${TABLE} USING btree(gFlux_PS);
CREATE INDEX ${TABLE}_idx_photo_r ON ${TABLE} USING btree(rFlux_PS);
CREATE INDEX ${TABLE}_idx_photo_i ON ${TABLE} USING btree(iFlux_PS);
CREATE INDEX ${TABLE}_idx_photo_z ON ${TABLE} USING btree(zFlux_PS);
CREATE INDEX ${TABLE}_idx_photo_y ON ${TABLE} USING btree(yFlux_PS);

EOF

done << EOF
   ${POOL3}   001
   ${POOL1}   002
   ${POOL1}   003
   ${POOL4}   004
   ${POOL2}   005
   ${POOL3}   006
   ${POOL2}   007
   ${POOL4}   008
   ${POOL1}   009
   ${POOL2}   010
   ${POOL3}   011
   ${POOL3}   012
   ${POOL3}   013
   ${POOL1}   014
   ${POOL3}   015
   ${POOL4}   016
   ${POOL2}   017
   ${POOL3}   018
EOF

while read PORT CHUNK
do

TABLE=source_${CHUNK}_xyz

echo == CREATING index for ${TABLE} on port ${PORT} ==

psql -p ${PORT} postgres << EOF

CREATE INDEX ${TABLE}_idx_sourceid ON ${TABLE} USING btree(sourceid);
CREATE INDEX ${TABLE}_idx_objectid ON ${TABLE} USING btree(objectid);

EOF

done << EOF
   ${POOL3}   001
   ${POOL1}   002
   ${POOL1}   003
   ${POOL4}   004
   ${POOL2}   005
   ${POOL3}   006
   ${POOL2}   007
   ${POOL4}   008
   ${POOL1}   009
   ${POOL2}   010
   ${POOL3}   011
   ${POOL3}   012
   ${POOL3}   013
   ${POOL1}   014
   ${POOL3}   015
   ${POOL4}   016
   ${POOL2}   017
   ${POOL3}   018
EOF


while read PORT CHUNK
do

TABLE=object_${CHUNK}_xyz

echo == ANALYZING ${TABLE} on port ${PORT} ==

psql -p ${PORT} postgres << EOF

ANALYZE ${TABLE} ;

EOF

TABLE=source_${CHUNK}_xyz

echo == ANALYZING ${TABLE} on port ${PORT} ==

psql -p ${PORT} postgres << EOF

ANALYZE ${TABLE} ;

EOF

done << EOF
   ${POOL3}   001
   ${POOL1}   002
   ${POOL1}   003
   ${POOL4}   004
   ${POOL2}   005
   ${POOL3}   006
   ${POOL2}   007
   ${POOL4}   008
   ${POOL1}   009
   ${POOL2}   010
   ${POOL3}   011
   ${POOL3}   012
   ${POOL3}   013
   ${POOL1}   014
   ${POOL3}   015
   ${POOL4}   016
   ${POOL2}   017
   ${POOL3}   018
EOF






