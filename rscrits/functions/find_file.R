##-------------------------------------------
## Author  : Izi (izi31416@protonmail.com)
## Project :
## Created : seg 26 mai 2025 17:29:56
## License :
## Updated :
##-------------------------------------------
find_file = function(x, path, pattern = "\\.rsx$")
{
  list_file = list.files(path, pattern = pattern, recursive = TRUE, full.names = TRUE)
  arq = grep(paste0(x, collapse="|"), list_file, value = TRUE)
  arq
}
