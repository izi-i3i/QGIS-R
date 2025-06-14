#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : sex 30 mai 2025 01:37:43
# License :
# Updated :
#-------------------------------------------
is_crs_planar = function(x)
{
  crs_layer = raster::crs(x)
  crs_unit = st_crs(crs_layer, parameters = TRUE)$units_gdal
  crs_unit == "metre"
}
