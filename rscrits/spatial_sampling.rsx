#' Layer_vector: the vector layer is used
#' Layer_raster: the vector raster is used
#' Type: Random for completely spatial random;<br/>
#'     : Regular for regular (systematically aligned) sampling;<br/>
#'     : Stratified for stratified random (one single random location in each cell);<br/>
#'     : Nonaligned for nonaligned systematic sampling (nx random y coordinates, ny random x coordinates);<br/>
#'     : Hexagonal for sampling on a hexagonal lattice;<br/>
#'     : Clustered for clustered sampling.
#' Size: Sample size (approximate).
#' Iteraction: Number of times to try to place sample points in<br/>
#'           : a polygon before giving up and returning NULL - this may occur when<br/>
#'           : trying to hit a small and awkwardly shaped polygon in a large<br/>
#'           : bounding box with a small number of points<br/>
#' Cluster: Number clusters
#' Set_seed: set seed
#' ALG_DESC: This file creates a simple vector
#' ALG_CREATOR: Izi i3i
#' ALG_HELP_CREATOR: i3i
#' ALG_VERSION: 0.0.1

##[Spatial Points]=group
##Spatial sampling=name
##QgsProcessingParameterFeatureSource|Layer_vector|Vector|-1|None|True
##QgsProcessingParameterRasterLayer|Layer_raster|Raster|None|True
##Extent=extent
##Type=enum Random;Regular;Stratified;Nonaligned;Hexagonal;Clustered ;
##QgsProcessingParameterNumber|Size|Size|QgsProcessingParameterNumber.Integer|100
##QgsProcessingParameterNumber|Clusters|Clusters|QgsProcessingParameterNumber.Integer|5
#QgsProcessingParameterNumber|Seed|Seed|QgsProcessingParameterNumber.Integer|None
##QgsProcessingParameterNumber|Iter|Iteraction|QgsProcessingParameterNumber.Integer|4
###Set_seed=optional number
##Spatial_sampling=output vector
if(is.null(Layer_raster) & is.null(Layer_vector)) {stop("Insert a layer RASTER or VECTOR")}

if(!is.null(Layer_vector))
{
  Layer = as_Spatial(Layer_vector)
  cat("used vector layer!")
} else {
  Layer <- as(Layer_raster, 'SpatialPixelsDataFrame')
    cat("used raster layer!")
}

if(!is.null(Layer_vector) & !is.null(Layer_raster)){
  cat("used vector layer!")
  }

Layer = crop(Layer, Extent)

# Identify sample type
Type <- c("random", "regular", "stratified", "nonaligned", "hexagonal", "clustered")[Type + 1]

# Sample
if(!is.null(Set_seed)) set.seed(Set_seed)
Output <- sp::spsample(x = Layer, n = Size, type = Type, iter = Iter, nclusters = Clusters)
Spatial_sampling = st_as_sf(Output)
