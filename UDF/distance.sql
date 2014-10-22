
CREATE FUNCTION spherical_distance(
   ra1 double precision, decl1 double precision,
   ra2 double precision, decl2 double precision)
RETURNS double precision
AS
$code$
DECLARE A_X double precision ;
        A_Y double precision ;
        A_Z double precision ;
        B_X double precision ;
        B_Y double precision ;
        B_Z double precision ;
        TMP double precision ;
BEGIN
  A_X = cos(RADIANS(ra1)) * cos(RADIANS(decl1)) ;
  A_Y = sin(RADIANS(ra1)) * cos(RADIANS(decl1)) ;
  A_Z = sin(RADIANS(decl1)) ;
  
  B_X = cos(RADIANS(ra2)) * cos(RADIANS(decl2)) ;
  B_Y = sin(RADIANS(ra2)) * cos(RADIANS(decl2)) ;
  B_Z = sin(RADIANS(decl2)) ;
  
  TMP = (A_X - B_X)^2 + (A_Y - B_Y)^2 + (A_Z - B_Z)^2 ;
  
  RETURN 2*asin(sqrt(TMP)/2) ;
END;
$code$
LANGUAGE PLPGSQL ;
