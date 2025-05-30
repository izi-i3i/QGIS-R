#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : segunda nov 16, 2024 14:09:15 -03
# License :
# Updated :
#-------------------------------------------
get_grid = function (layer,
                     resolution,
                     grid.method = c('Rectangle','Convex hull'),
                     expand = TRUE,
                     fx = 0.01,
                     fy = 0.01
){
  if(class(layer)[1]=="sf") layer = as_Spatial(layer)
  grid.method = match.arg(grid.method)

           x1 = layer@bbox[1]
           x2 = layer@bbox[3]
           y1 = layer@bbox[2]
           y2 = layer@bbox[4]

           if(expand)
           {
             x1 = x1 - (x2 - x1) * fx
             x2 = x2 + (x2 - x1) * fx
             y1 = y1 - (y2 - y1) * fy
             y2 = y2 + (y2 - y1) * fy
           }

  switch(grid.method,
         Rectangle = {
           bottomright = c(x1, y1)
           topleft = c(x2, y2)
           pol = c(topleft[1],bottomright[1],
                    bottomright[1],topleft[1],
                    topleft[1], topleft[2],
                    topleft[2], bottomright[2],
                    bottomright[2],topleft[2])
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

#   gride = makegrid(d, n = 1, cellsize=c(resolution,resolution))
#   names(gride) <- c("x","y")
#   gridded(gride) = ~x+y
  attr(gride, "proj4string") = layer@proj4string
  return(gride)
}

