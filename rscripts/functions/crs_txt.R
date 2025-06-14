#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : sex 06 jun 2025 13:22:50
# License :
# Updated :
#-------------------------------------------
crs_txt = function(x)
{
  crs_info = st_crs(x)
  crs_num = crs_info$epsg
  crs_proj = st_crs(st_sfc(crs = crs_num))
  epsg_crs_txt = paste0("EPSG:", crs_num, " ", crs_proj$Name)
  epsg_crs_txt
}
