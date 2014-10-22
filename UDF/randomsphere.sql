CREATE OR REPLACE FUNCTION create_random_sphere(nbpoints bigint)
RETURNS VOID
AS $$
DROP TABLE IF EXISTS random_sphere ;
CREATE TEMPORARY TABLE random_sphere AS
  SELECT pointid,
         sign(Y) * degrees(acos(X / sqrt(X*X + Y*Y + Z*Z))) AS ra,
         degrees(asin(Z / sqrt(X*X + Y*Y + Z*Z))) AS decl
  FROM
  ( SELECT pointid, 2*random() - 1. as X, 2*random() - 1. as Y, 2*random() - 1. as Z
    FROM (SELECT * FROM generate_series(1,nbpoints) AS pointid) as _1 ) as _2
  WHERE X*X + Y*Y + Z*Z > 0. AND X*X + Y*Y + Z*Z < 1. ;
$$ language SQL ;

