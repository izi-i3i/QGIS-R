#-------------------------------------------
# Author  : Izi (izi31416@protonmail.com)
# Project :
# Created : segunda nov 16, 2024 14:09:15 -03
# License :
# Updated :
#-------------------------------------------
get_grid = function (x,
                     extent = NULL,
                     mask.layer = NULL,
                     resolution,
                     grid.method = c('Rectangle', 'Polygon', 'Convex hull'),
                     expand = FALSE,
                     fx = 0.01,
                     fy = 0.01
){
  if(class(x)[1] == "sf") x = as_Spatial(x)
  grid.method = match.arg(grid.method)

  if(is.null(extent))
  {
    x1 = x@bbox[1]
    x2 = x@bbox[3]
    y1 = x@bbox[2]
    y2 = x@bbox[4]
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

  rect_pol = function()
  {
    #   (x1,y2)-----------(x2,y2)
    #      |                 |
    #      |                 |
    #      |                 |
    #      |                 |
    #      |                 |
    #   (x1,y1)-----------(x2,y1)
    pol = c(x2, x1, x1, x2, x2,
            y2, y2, y1, y1, y2)
    coord_matrix = matrix(pol, ncol = 2, nrow = 5)
    rp = SpatialPolygons(list(Polygons(list(Polygon(coords = coord_matrix)), ID = 1)))
    rp
  }

  switch(grid.method,
         'Polygon' = {
             st_agr(mask.layer) = "constant"
             mask_layer = st_crop(mask.layer, xmin = x1, ymin = y1, xmax = x2, ymax = y2)
             d = as(mask_layer, "Spatial")
         },
         'Rectangle' = {
           d = rect_pol()
         },
         'Convex hull' = {
           convex_hull = chull(coordinates(x)[, 1], coordinates(x)[,2])
           convex_hull = c(convex_hull, convex_hull[1])
           ch = x[convex_hull, ]
           d = Polygon(ch)
         })

  gride = spsample(d, n=1, cellsize=c(resolution,resolution), type = "regular", pretty = FALSE)

  attr(gride@coords, "dimnames")[[2]] <- c("x", "y")
  attr(gride@bbox, "dimnames")[[1]] <- c("x", "y")
  gridded(gride) = TRUE
  attr(gride, "proj4string") = x@proj4string

  return(gride)
}

