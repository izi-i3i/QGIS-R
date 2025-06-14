#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : qui 05 jun 2025 02:23:12
# License :
# Updated :
#-------------------------------------------
palette_colors = function(name = "Spectral", n = 100, reverse = FALSE)
{
  palColor = list(
      Regions = c("#000033", "#0000CC", "#5000FF", "#C829D7", "#FF758A", "#FFC23E", "#FFFF60"),
      Turbo = c("#30123B", "#4686FB", "#1AE4B6", "#A2FC3C", "#FABA39", "#E4460A", "#7A0403"),
      Magma = c("#000004", "#2D1160", "#721F81", "#B63679", "#F1605D", "#FEAF77", "#FCFDBF")
    )[[name]]

  if(is.null(palColor))
  {
    Colors = hcl.colors(n = n, palette = name)
  } else  {
    Colors = as.character(pal_ramp(palette = pal_color(palColor), n = n, direction = 1))
  }
  out = if(reverse) rev(Colors) else Colors
  return(out)
}
