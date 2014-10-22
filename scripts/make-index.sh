#!/bin/bash

DBCLIENT=$1
CHUNK=$2

# Object

TABLE=object_${CHUNK}_xyz

echo == INDEXING ${TABLE} ==


${DBCLIENT} <<EOF
\timing

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

CREATE INDEX ${TABLE}_idx_photo_u ON ${TABLE} USING btree(uFlux_PS);
CREATE INDEX ${TABLE}_idx_photo_g ON ${TABLE} USING btree(gFlux_PS);
CREATE INDEX ${TABLE}_idx_photo_r ON ${TABLE} USING btree(rFlux_PS);
CREATE INDEX ${TABLE}_idx_photo_i ON ${TABLE} USING btree(iFlux_PS);
CREATE INDEX ${TABLE}_idx_photo_z ON ${TABLE} USING btree(zFlux_PS);
CREATE INDEX ${TABLE}_idx_photo_y ON ${TABLE} USING btree(yFlux_PS);

CLUSTER ${TABLE} USING ${TABLE}_idx_xyz ;

ANALYZE ${TABLE} ;

EOF

# Source

TABLE=source_${CHUNK}_xyz

echo == INDEXING ${TABLE} ==

${DBCLIENT} <<EOF
\timing
CREATE INDEX ${TABLE}_idx_oid ON ${TABLE} USING btree(objectid);
CREATE INDEX ${TABLE}_idx_sid ON ${TABLE} USING btree(sourceid);

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

ANALYZE ${TABLE} ;

EOF

