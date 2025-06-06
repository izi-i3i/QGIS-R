#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : sáb 24 mai 2025 23:23:15
# License :
# Updated :
#-------------------------------------------
change_dir = function(arq, dir_path, dir_path_aux, pattern = "\\.rsx$")
{
  h1 = "##QgsProcessingParameterFile|rscripts_folder|Path to rscript folder|1||"
  h2 = "|True"

  old_dir = paste0(h1, dir_path_aux, h2)
  new_dir = paste0(h1, dir_path, h2)

  if (new_dir != old_dir)
  {
    list_files = list.files(dir_path, pattern = pattern, recursive = TRUE, full.names = TRUE)
    fp = grep(paste0(arq, collapse="|"), list_files, value = TRUE)
#     fp = find_file(arq, dir_path, pattern = pattern)
    txt = readLines(fp)
    txt[txt == old_dir] <- new_dir

    fileConn = file(fp)
    writeLines(txt, fileConn)
    close(fileConn)
    cat(" ----------------------------------\n")
    cat("  Path to rscript folder - Updated!\n\n")
  }
}
