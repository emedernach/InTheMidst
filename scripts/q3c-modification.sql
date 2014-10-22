
--   Fonctions de librairie (A REMPLACER):
--   - q3c_dist
--   - q3c_radial_query_it
--   - (Ok) q3c_sindist
--   - q3c_ang2ipix
--   - q3c_nearby_it
--   
--   q3c_radial_query utilise:
--   - q3c_radial_query_it
--   - (Ok) q3c_sindist
--
--   q3c_join utilise:
--   - q3c_ang2ipix
--   - q3c_nearby_it

CREATE OR REPLACE FUNCTION q3c_sindist(ra1 double precision, decl1 double precision,
                                       ra2 double precision, decl2 double precision)
 RETURNS double precision AS $$
 DECLARE X double precision ;
         Y double precision ;
         Z double precision ;
         Q3C_DEGRA double precision ;  
 BEGIN
   Q3C_DEGRA = 0.01745329251994329576923690768488612L ;
   X = sin ((ra1 - ra2) / 2 * Q3C_DEGRA);
   X = X*X ;
   Y = sin ((decl1 - decl2) / 2 * Q3C_DEGRA);
   Y = Y*Y ;
   Z = cos ((decl1 + decl2)/2 * Q3C_DEGRA);
   Z = Z*Z ;
   RETURN X * (Z - Y) + Y;
 END;
 $$ LANGUAGE plpgsql;

-- Ca ne marche pas: il continue de faire  Foreign Scan + Filter au lieu d'envoyer la requete !


 