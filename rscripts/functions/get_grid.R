#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : segunda nov 16, 2024 14:09:15 -03
# License :
# Updated :
#-------------------------------------------
get_grid = function (layer,
                     extent = NULL,
                     resolution,
                     grid.method = c('Rectangle','Convex hull'),
                     expand = FALSE,
                     fx = 0.01,
                     fy = 0.01
){
  if(class(layer)[1]=="sf") layer = as_Spatial(layer)
  grid.method = match.arg(grid.method)

  if(is.null(extent))
  {
    x1 = layer@bbox[1]
    x2 = layer@bbox[3]
    y1 = layer@bbox[2]
    y2 = layer@bbox[4]
  } else {
    x1 = extent[1]
    x2 = extent[2]
    y1 = extent[3]
    y2 = extent[4]
  }

  if(expand)
  {
    x1 = x1 - (x2 - x1) * fx
    x2 = x2 + (x2 - x1) * fx
    y1 = y1 - (y2 - y1) * fy
    y2 = y2 + (y2 - y1) * fy
  }

#   (x1,y2)--------------(x2,y2)
#      |                    |
#      |                    |
#      |                    |
#      |                    |
#      |                    |
#   (x1,y1)--------------(x2,y1)

  switch(grid.method,
         'Rectangle' = {
           pol = c(x2, x1, x1, x2, x2,
                   y2, y2, y1, y1, y2)
           coord_matrix = matrix(pol, ncol=2, nrow=5)
           d = SpatialPolygons(list(Polygons(list(Polygon(coords = coord_matrix)), ID=1)))
         },
         'Convex hull' = {
           convex_hull = chull(coordinates(layer)[, 1], coordinates(layer)[,2])
           convex_hull = c(convex_hull, convex_hull[1])
           ch = layer[convex_hull, ]
           d = Polygon(ch)
         })

  gride = spsample(d, n=1, cellsize=c(resolution,resolution), type="regular")
  attr(gride@coords, "dimnames")[[2]] <- c("x", "y")
  attr(gride@bbox, "dimnames")[[1]] <- c("x", "y")
  gridded(gride) = T

  attr(gride, "proj4string") = layer@proj4string
  return(gride)
}

