
CREATE OR REPLACE FUNCTION angular_distance(
  ra1 double precision,
  decl1 double precision,
  ra2 double precision,
  decl2 double precision)
RETURNS DOUBLE PRECISION
AS $$
SELECT
degrees(2*asin(sqrt(sin(radians((decl2 - decl1)/2))^2 +
        sin(radians((ra2 - ra1)/2))^2 * (cos(radians((decl2 + decl1)/2))^2 -
        sin(radians((decl2 - decl1)/2))^2)))) ;
$$ language SQL immutable ;        

CREATE OR REPLACE FUNCTION angular_distance(
  ra1 double precision,
  decl1 double precision,
  ra2 double precision,
  decl2 double precision)
RETURNS DOUBLE PRECISION
AS $$
SELECT
degrees(
 2*asin(sqrt(
  power(sin(radians((decl2 - decl1)/2)), 2) +
  power(sin(radians((ra2 - ra1)/2)), 2) *
   cos(radians(decl1)) * cos(radians(decl2)) 
 ))) ;
$$ language SQL immutable ;        

DROP FUNCTION conesearch(
double precision,double precision,
double precision,double precision,
double precision);

CREATE FUNCTION conesearch(
  ra1 double precision,
  decl1 double precision,
  ra2 double precision,
  decl2 double precision,
  radius double precision)
RETURNS  boolean AS $$
-- radius is in degrees and shoulb be converted to radians.
SELECT cos(radians(ra1))*cos(radians(decl1))
    BETWEEN cos(radians(ra2))*cos(radians(decl2)) - 2*sin(radians(radius/2))
        AND cos(radians(ra2))*cos(radians(decl2)) + 2*sin(radians(radius/2))
    AND sin(radians(ra1))*cos(radians(decl1))
    BETWEEN sin(radians(ra2))*cos(radians(decl2)) - 2*sin(radians(radius/2))
        AND sin(radians(ra2))*cos(radians(decl2)) + 2*sin(radians(radius/2))
    AND sin(radians(decl1))
    BETWEEN sin(radians(decl2)) - 2*sin(radians(radius/2))
        AND sin(radians(decl2)) + 2*sin(radians(radius/2))
    AND
    degrees(
      2*asin(sqrt(
       power(sin(radians((decl2 - decl1)/2)), 2) +
       power(sin(radians((ra2 - ra1)/2)), 2) *
        cos(radians(decl1)) * cos(radians(decl2)) 
     ))) <= radius
$$ language sql immutable ;

----

CREATE OR REPLACE FUNCTION BoundaryXYZ(
  ra   double precision,
  decl double precision,
  xmin double precision,
  xmax double precision,
  ymin double precision,
  ymax double precision,
  zmin double precision,
  zmax double precision,
  radius double precision
 )
RETURNS boolean AS $$
SELECT cos(radians(ra))*cos(radians(decl))
    BETWEEN xmin + 2*sin(radians(radius/2))
        AND xmax - 2*sin(radians(radius/2))
    AND sin(radians(ra))*cos(radians(decl))
    BETWEEN ymin + 2*sin(radians(radius/2))
        AND ymax - 2*sin(radians(radius/2))
    AND sin(radians(decl))
    BETWEEN zmin + 2*sin(radians(radius/2))
        AND zmax - 2*sin(radians(radius/2))
$$ language sql immutable ;

CREATE OR REPLACE FUNCTION CreateObjectBoundary( radius double precision )
RETURNS VOID
AS $$
DROP TABLE IF EXISTS ObjectBoundary ;

CREATE TABLE ObjectBoundary AS
SELECT * FROM    master_object_001_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,    -0.05, 0.00,    -0.15, -0.10,    radius)
UNION ALL 
SELECT * FROM    master_object_002_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,    -0.05, 0.00,     -0.1, -0.05,    radius)
UNION ALL 
SELECT * FROM    master_object_003_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,    -0.05, 0.00,    -0.05,  0.00,    radius)
UNION ALL 
SELECT * FROM    master_object_004_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,    -0.05, 0.00,        0,  0.05,    radius)
UNION ALL 
SELECT * FROM    master_object_005_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,    -0.05, 0.00,     0.05,  0.10,    radius)
UNION ALL 
SELECT * FROM    master_object_006_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,    -0.05, 0.00,      0.1,  0.15,    radius)
UNION ALL 
SELECT * FROM    master_object_007_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,        0, 0.05,    -0.15, -0.10,    radius)
UNION ALL 
SELECT * FROM    master_object_008_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,        0, 0.05,     -0.1, -0.05,    radius)
UNION ALL 
SELECT * FROM    master_object_009_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,        0, 0.05,    -0.05,  0.00,    radius)
UNION ALL 
SELECT * FROM    master_object_010_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,        0, 0.05,        0,  0.05,    radius)
UNION ALL 
SELECT * FROM    master_object_011_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,        0, 0.05,     0.05,  0.10,    radius)
UNION ALL 
SELECT * FROM    master_object_012_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,        0, 0.05,      0.1,  0.15,    radius)
UNION ALL 
SELECT * FROM    master_object_013_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,     0.05, 0.10,    -0.15, -0.10,    radius)
UNION ALL 
SELECT * FROM    master_object_014_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,     0.05, 0.10,     -0.1, -0.05,    radius)
UNION ALL 
SELECT * FROM    master_object_015_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,     0.05, 0.10,    -0.05,  0.00,    radius)
UNION ALL 
SELECT * FROM    master_object_016_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,     0.05, 0.10,        0,  0.05,    radius)
UNION ALL 
SELECT * FROM    master_object_017_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,     0.05, 0.10,     0.05,  0.10,    radius)
UNION ALL 
SELECT * FROM    master_object_018_xyz WHERE NOT BoundaryXYZ(ra_PS, decl_PS,  0.95, 1.00,     0.05, 0.10,      0.1,  0.15,    radius)
;

CREATE INDEX ObjectBoundary_idx_id ON ObjectBoundary USING btree(objectid);

CREATE INDEX ObjectBoundary_idx_xyz ON ObjectBoundary
USING btree ((cos(radians(ra_PS))*cos(radians(decl_PS))),
             (sin(radians(ra_PS))*cos(radians(decl_PS))),
             (sin(radians(decl_PS))));

CREATE INDEX ObjectBoundary_idx_ra   on ObjectBoundary using btree (ra_PS) ;
CREATE INDEX ObjectBoundary_idx_decl on ObjectBoundary using btree (decl_PS) ;  

CREATE INDEX ObjectBoundary_idx_X on ObjectBoundary using btree ((cos(radians(ra_PS))*cos(radians(decl_PS)))) ;
CREATE INDEX ObjectBoundary_idx_Y on ObjectBoundary using btree ((sin(radians(ra_PS))*cos(radians(decl_PS)))) ;  
CREATE INDEX ObjectBoundary_idx_Z on ObjectBoundary using btree ((sin(radians(decl_PS)))) ;  

CLUSTER ObjectBoundary USING ObjectBoundary_idx_xyz ;

ANALYZE ObjectBoundary ;

$$ language SQL ;


CREATE OR REPLACE FUNCTION CreateSourceBoundary( radius double precision )
RETURNS VOID
AS $$
DROP TABLE IF EXISTS SourceBoundary ;

CREATE TABLE SourceBoundary AS
SELECT * FROM    master_source_001_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,    -0.05, 0.00,    -0.15, -0.10,    radius)
UNION ALL 
SELECT * FROM    master_source_002_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,    -0.05, 0.00,     -0.1, -0.05,    radius)
UNION ALL 
SELECT * FROM    master_source_003_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,    -0.05, 0.00,    -0.05,  0.00,    radius)
UNION ALL 
SELECT * FROM    master_source_004_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,    -0.05, 0.00,        0,  0.05,    radius)
UNION ALL 
SELECT * FROM    master_source_005_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,    -0.05, 0.00,     0.05,  0.10,    radius)
UNION ALL 
SELECT * FROM    master_source_006_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,    -0.05, 0.00,      0.1,  0.15,    radius)
UNION ALL 
SELECT * FROM    master_source_007_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,        0, 0.05,    -0.15, -0.10,    radius)
UNION ALL 
SELECT * FROM    master_source_008_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,        0, 0.05,     -0.1, -0.05,    radius)
UNION ALL 
SELECT * FROM    master_source_009_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,        0, 0.05,    -0.05,  0.00,    radius)
UNION ALL 
SELECT * FROM    master_source_010_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,        0, 0.05,        0,  0.05,    radius)
UNION ALL 
SELECT * FROM    master_source_011_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,        0, 0.05,     0.05,  0.10,    radius)
UNION ALL 
SELECT * FROM    master_source_012_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,        0, 0.05,      0.1,  0.15,    radius)
UNION ALL 
SELECT * FROM    master_source_013_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,     0.05, 0.10,    -0.15, -0.10,    radius)
UNION ALL 
SELECT * FROM    master_source_014_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,     0.05, 0.10,     -0.1, -0.05,    radius)
UNION ALL 
SELECT * FROM    master_source_015_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,     0.05, 0.10,    -0.05,  0.00,    radius)
UNION ALL 
SELECT * FROM    master_source_016_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,     0.05, 0.10,        0,  0.05,    radius)
UNION ALL 
SELECT * FROM    master_source_017_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,     0.05, 0.10,     0.05,  0.10,    radius)
UNION ALL 
SELECT * FROM    master_source_018_xyz WHERE NOT BoundaryXYZ(ra, decl,  0.95, 1.00,     0.05, 0.10,      0.1,  0.15,    radius)
;

CREATE INDEX SourceBoundary_idx_objectid ON SourceBoundary USING btree(objectid);
CREATE INDEX SourceBoundary_idx_sourceid ON SourceBoundary USING btree(sourceid);

CREATE INDEX SourceBoundary_idx_ra   on SourceBoundary using btree (ra) ;
CREATE INDEX SourceBoundary_idx_decl on SourceBoundary using btree (decl) ;  

CREATE INDEX SourceBoundary_idx_X on SourceBoundary using btree ((cos(radians(ra))*cos(radians(decl)))) ;
CREATE INDEX SourceBoundary_idx_Y on SourceBoundary using btree ((sin(radians(ra))*cos(radians(decl)))) ;  
CREATE INDEX SourceBoundary_idx_Z on SourceBoundary using btree ((sin(radians(decl)))) ;  

-- Of course we could add others ra/decl as raAstrom,declAstron, etc.
CREATE INDEX SourceBoundary_idx_xyz ON SourceBoundary
USING btree ((cos(radians(ra))*cos(radians(decl))),
             (sin(radians(ra))*cos(radians(decl))),
             (sin(radians(decl))));

CLUSTER SourceBoundary USING SourceBoundary_idx_xyz ;

ANALYZE SourceBoundary ;
  
$$ language SQL ;

-- After creating all chunks don't forget to create boundaries tables !

-- With a max radius of 0.01 for instance :
-- SELECT CreateObjectBoundary( 0.01 ); select count(*) from ObjectBoundary;
-- SELECT CreateSourceBoundary( 0.01 ); select count(*) from SourceBoundary;


