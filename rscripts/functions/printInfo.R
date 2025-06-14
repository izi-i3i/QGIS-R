#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : seg 02 jun 2025 18:01:30
# License :
# Updated :
#-------------------------------------------
printInfo <- function()
{
  #TODO: icluir versão do qgis
  cat("Create:", format(Sys.time(), "%Y-%m-%d %X %Z"), "\n")
  cat("QGIS version:", qgis_version, "\n")
  cat("Resolution:", Resolution,"meter\n")
  cat("Logarithm field:", Log_Field, "\n")
  cat("Field:", Field, "\n")
  cat("Formula:", gsub("Field", Field, form), "\n")
  cat("Seed:", ifelse(is.null(Set_Seed), "NULL", Set_Seed), "\n")
  cat("Grid method:", Grid_method, "\n")
  cat("Block size:", Block_size, "\n")
  cat("Model:", mt_select, "\n")
  cat("Auto fit variogram:", Auto_fit_variogram, "\n")
  cat("Estimate Range_and Psill:", Estimate_Range_and_Psill, "\n")
  cat("Nugget:", Nugget, "\n")
  cat("Range:", Range, "\n")
  cat("Psill:", Psill, "\n")
  cat("Maximum:", Maximum, "\n")
  cat("Minimum:", Minimum, "\n")
  cat("N-fold:", N_fold, "\n")
  cat("Color Ramp Report:", Color_Ramp_Report, "\n")
  cat("Invert Color Ramp:", Invert_Color_Ramp, "\n")
  cat("Create Report:", Create_Report, "\n")
  cat("Open Report:", Open_Report, "\n")
  cat("Insert points:", Insert_points, "\n")
  cat("Draw lines variogram:", Draw_lines_variogram, "\n")
  cat("Plot contour report:", Plot_contour_report, "\n")
  cat("Plot_mask:", Plot_mask, "\n")
  cat("Mask conf report:", Mask_conf_report, "\n")
  cat("Contour conf report:", Contour_conf_report, "\n")
  cat("Extent:", paste(round(c(Extent[1], Extent[2], Extent[3], Extent[4])), collapse=", "), "\n")
  cat(crs_txt(LAYER), "\n")
  cat("rscript folder:", dir_path, "\n\n")
}
