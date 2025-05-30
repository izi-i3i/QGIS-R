#' ALG_DESC: <p>This file creates a <span style='text-decoration: underline;'>Ordinary Kriging</span>.
#'         : This script does Ordinary Kriging interpolation from a numeric field of a points vector layer.
#'         : It allows to auto select the initial values for nugget, psill and range; or it can fit a model
#'         : from initial values provided. Besides, you can limit the number of points used to predict.</p>
#' Layer: Points vector layer.
#' CRS_Layer: Decimal degree coordinate is transformed to planar coordinate.
#' Field: numeric field from layer to interpolate.
#' Log_Field: If checked, logarithmize the vector values. Field = log(Field)
#' Extent: Specifies a numeric variable of length 4 values (xmin, xmax, ymin and ymax).
#' Grid_method: Method to calculate the extent of interpolation.
#'            : Rectangle = rectangular layer,
#'            : Convex hull = Compute Convex Hull of a set of points.
#' Expand_vector: If checked, expands the longitude and latitude (only in rectangle) values by the value in
#'              : Expand_longitude and Expand_latitude
#' Expand_longitude: value added to longitude.
#' Expand_latitude: value added to latitude.
#' Model: Spherical (Sph), Exponential (Exp), Gaussian (Gau), Matern (Mat),
#'      : Matern Stein's parameterization (Ste), Exponential class (Exc),
#'      : Circular (Cir), Linear (Lin), Bessel (Bes), Pentaspherical (Pen),
#'      : Periodic (Per), Wave (Wave), Hole (Hol), Logarithmic (Log),
#'      : Spline (Spl), Power (Pow), Nugget (Nug)
#'      : in which case the best fitting is returned.
#' Auto_fit_variogram: Automatically fitting a variogram to the data on which it is applied.
#' Estimate_Range_and_Psill: If checked, initial values for nugget, psill and range will be
#'                         : estimated from sample variogram.
#' Nugget: Iniital value for nugget
#' Range: Initial value for range
#' Psill: Initial value for partial sill
#' Local_kriging: If checked, points to interpolate will be limited to a number of nearest observations.
#' Number_of_nearest_observations: Maximun number of observations used in local kriging.
#' Resolution: If the value is zero it will be calculated automatically, in meters.
#' Set_Seed: Ensures that the same random values are produced every time you run the code
#'         : In blank the number generation is random.
#' Create_report: Create report with graphs.
#' Open_report: Open report.
#' Color_report: Select color palette: Turbo, Spectral, Magma, Inferno, Plasma, viridis, Cividis, Rocket, Mako.
#' N_colors: Number of color ramp.
#' Invert_Color_Ramp: Invert color ramp.
#' Draw_lines_variogram: Draw lines variogram.
#' Report: Directory and name of the report (docx) to be saved.
#' rscripts_folder: path to rscript folder.
#' OK_variance: Kriging variance of prediction (raster)
#' OK_prediction: Kriging predicted value (raster)
#' ALG_CREATOR: <a href='https://github.com/izi-i3i/QGIS-R/'>izi-i3i</a>
#' ALG_HELP_CREATOR: izi-i3i
#' ALG_VERSION: 0.0.8

##Ordinary Kriging=name
##[R-Geostatistics]=group
##QgsProcessingParameterFeatureSource|Layer|Layer vector|0|None|False
##QgsProcessingParameterCrs|CRS_Layer|CRS Layer (Planar coordenates)|EPSG:3857
##QgsProcessingParameterFeatureSource|Mask_layer|Mask Layer|2|None|True
##Field=Field Layer
##Log_Field=boolean False
##Extent=extent
##CRS_Extent=expression @project_crs
##Grid_method=enum literal Rectangle;Convex hull ;
##Expand_vector=boolean True
##QgsProcessingParameterNumber|Expand_longitude|Expand longitude (only rectangle)|QgsProcessingParameterNumber.Double|0.1
##QgsProcessingParameterNumber|Expand_latitude|Expand latitude (only rectangle)|QgsProcessingParameterNumber.Double|0.1
##Model=enum multiple Spherical (Sph);Exponential (Exp);Gaussian (Gau);Matern (Mat); Matern Stein's parameterization (Ste);Exponential class (Exc);Circular (Cir);Linear (Lin);Bessel (Bes);Pentaspherical (Pen);Periodic (Per);Wave (Wave);Hole (Hol);Logarithmic (Log);Spline (Spl);Power (Pow);Nugget (Nug)
##Auto_fit_variogram=boolean True
##Estimate_Range_and_Psill=boolean True
##Nugget=number 0
##Range=number 0
##Psill=number 0
##Local_kriging=boolean False
##QgsProcessingParameterNumber|Nearest_observations|Number of nearest observations|QgsProcessingParameterNumber.Integer|25
##Resolution=string "auto"
##Set_Seed=string "1234"
##Create_report=boolean True
##Open_report=boolean False
##Color_report=enum literal Turbo;Spectral;Magma;Inferno;Plasma;Viridis;Cividis;Rocket;Mako ;
##QgsProcessingParameterNumber|N_colors|Number of colors ramp|QgsProcessingParameterNumber.Integer|10
##Invert_Color_Ramp=boolean False
##Insert_points=boolean True
##Draw_lines_variogram=boolean True
##Report=output file docx
##QgsProcessingParameterFile|rscripts_folder|Path to rscript folder|1||~/.local/share/QGIS/QGIS3|True
##OK_variance=output raster
##OK_prediction=output raster
##OK_clip_prediction=output raster

# OPTIONS =================================================================
options(scipen = 9999) # scientific notation

# READ PACKAGES ====================================
packages = c("gstat", "sp", "sf", "automap", "raster", "ggrepel", "palettes","paletteer",
             "officer", "cowplot", "viridis", "ggpmisc", "ggplot2")

for (pac in packages) {
  if (!suppressMessages(require(pac, character.only=TRUE, quietly=TRUE))) {
    install.packages(pac, repos = getOption("repos"), dependencies=TRUE)
    cat("package installed:", paste(pac, collapse = ", "),"\n")
  }
}

# DIR QGIS3 ===============================================================
if(!dir.exists(rscripts_folder))
  stop(sprintf("\nFolder path ('%s') does not exist!", rscripts_folder), call. = FALSE)

file_path = file.path(getwd(), ".rscript_path")

if (!file.exists(file_path))
{
  os <- .Platform$OS.type
  if(os == "unix"){
    dir_path = "~/.local/share/QGIS/QGIS3"
  } else if(os == "windows"){
    dir_path = "AppData\\Roaming\\QGIS\\QGIS3"
  } else if(os == "osx"){
    dir_path = "~/Library/Application\\ Support/QGIS/QGIS3"
  }

  fileConn<-file(".rscript_path")
  writeLines(dir_path, fileConn)
  close(fileConn)
}

if (file.exists(file_path) & rscripts_folder != "")
{ # file=exists; folder=non-empty
  dir_path <- rscripts_folder
  dir_path_aux <- readLines(file_path)
  if (dir_path != dir_path_aux)
  {
    fileConn<-file(".rscript_path")
    writeLines(dir_path, fileConn)
    close(fileConn)
  }
} else if (file.exists(file_path)) {
    dir_path = dir_path_aux = readLines(file_path)
  } else {
    dir_path_aux = rscripts_folder
    dir_path = readLines(file_path)
}

if(!dir.exists(dir_path))
  stop(sprintf("\nFolder path ('%s') does not exist!", dir_path), call. = FALSE)

# READ FUNCTIONS ==========================================================
sourceFun = function(x, path, ...)
{
  list_files = list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  arq = grep(paste0(x, collapse="|"), list_files, value = TRUE)
  cat("Loading required functions:\n")
  for (i in 1:length(arq))
  {
    cat(" ----------------------------------\n")
    cat(i,":", arq[i], "\n", sep="")
    source(arq[i])
  }
  cat(" ----------------------------------\n\n")
}

fun = c("get_grid.R",
        "plot_variogram.R",
        "create_report.R",
        "change_dir.R",
        "find_file.R",
        "round_df.R",
        "is_crs_planar",
        "get_stats.R")
sourceFun(fun, path = dir_path, trace = TRUE)

# CHANGE DIR ==============================================================
change_dir("ordinary_kriging.rsx", dir_path, dir_path_aux, pattern="\\.rsx$")

# SEED ====================================================================
if(Set_Seed == "") Set_Seed = NULL
set.seed(Set_Seed)

# COLOR ===================================================================
N_colors = 100
dct = if(Invert_Color_Ramp) -1 else 1
Colors = list(
     "Spectral" = paletteer::paletteer_c("grDevices::Spectral", N_colors, direction = dct),
     "Turbo" = paletteer::paletteer_c("viridis::turbo", N_colors, direction = dct),
     "Magma" = paletteer::paletteer_c("viridis::magma", N_colors, direction = dct),
     "Inferno" = paletteer::paletteer_c("viridis::inferno", N_colors, direction = dct),
     "Plasma" = paletteer::paletteer_c("viridis::plasma", N_colors, direction = dct),
     "Viridis" = paletteer::paletteer_c("viridis::viridis", N_colors, direction = dct),
     "Cividis" = paletteer::paletteer_c("viridis::cividis", N_colors, direction = dct),
     "Rocket" = paletteer::paletteer_c("viridis::rocket", N_colors, direction = dct),
     "Mako" = paletteer::paletteer_c("viridis::mako", N_colors, direction = dct)
     )
Color_report = as.character(Colors[[Color_report]])

# LAYER CRS-TRANSFORM =================================
if (is_crs_planar(Layer))
{
  CRS_Layer = raster::crs(Layer)
} else {
  Mask_layer = st_transform(Mask_layer, crs = CRS_Layer)
  Layer = st_transform(Layer, crs = CRS_Layer, agr = "constant")
}

# extract crs layer
crs_info = st_crs(Layer)
crs_num = crs_info$epsg

if (!is_crs_planar(Layer))
  stop(
    sprintf("CRS_Layer ('%s') is not a planar coordinate!\nChange to a planar coordinate, example EPSG:3857.",
            crs_num), call. = FALSE)

# INFO ============================================
crs_proj = st_crs(st_sfc(crs = crs_num))
epsg_crs_txt = paste0("CRS - ", "EPSG:", crs_num, " ", crs_proj$Name)

# CRS ==============================================
xy = data.frame(x=Extent[1:2], y=Extent[3:4])
L1 = st_as_sf(xy, coords = c("x", "y"), crs = CRS_Extent, agr = "constant")
L2 = st_transform(L1, crs = raster::crs(CRS_Layer), agr = "constant")
Extent = st_bbox(L2)[c('xmin', 'xmax', 'ymin', 'ymax')]

# LAYER ============================================
LAYER = as_Spatial(Layer)
LAYER = crop(LAYER, Extent)
raster::crs(LAYER) <- raster::crs(CRS_Layer)

names(LAYER)[names(LAYER) == Field] = "Field"
LAYER = remove.duplicates(LAYER)
LAYER = LAYER[!is.na(LAYER$Field),]
LAYER$Field = as.numeric(as.character(LAYER$Field))

if(Log_Field)
{
  LAYER$Field = log(LAYER$Field)
  Field = paste0("log(", Field,")")
}

# SELECT MODEL =====================================
model1 = c("Sph", "Exp", "Gau",
          "Mat", "Ste", "Exc",
          "Cir", "Lin", "Bes",
          "Pen", "Per", "Wav",
          "Hol", "Log", "Spl",
          "Pow", "Nug")
model = model1[sort(Model+1)]

model_type = c("Spherical (Sph)", "Exponential (Exp)", "Gaussian (Gau)",
               "Matern (Mat)", "Stein's parameterization (Ste)", "Exponential class (Exc)",
               "Circular (Cir)", "Linear (Lin)", "Bessel (Bes)",
               "Pentaspherical (Pen)", "Periodic (Per)", "Wave (Wave)",
               "Hole (Hol)", "Logarithmic (Log)", "Spline (Spl)",
               "Power (Pow)", "Nugget (Nug)")
names(model_type) <- model1
mt_select = model_type[sort(Model+1)]

pn = model %in% c("Pow", "Nug")
ln = length(model)

if(ln > 15 | sum(pn) > 0)
{
  if(ln < 15 & any(pn) & length(pn) < 3)
  {
    model = model[1]
  } else {
    model = model[!pn]
    mt_select = mt_select[!mt_select %in% c("Power", "Nugget")]
  }

  if(ln > 1)
  {
    warning(
      paste0("In selected model type:\nModel selected: '",
        paste(mt_select, collapse = ", "), "', consider running 'Power' or 'Nugget' separately."), call. = FALSE)
  }
}

# VARIOGRAM ========================================
f = 'Field~1'
frm = formula(f)
g = gstat(id = Field, formula = frm, data = LAYER)

if(Auto_fit_variogram)
{
  fit_var = autofitVariogram(frm,
                             LAYER,
                             model = model,
                             kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10), 
                             fix.values = c(NA,NA,NA),
                             verbose = F,
                             GLS.model = NA,
                             start_vals = c(NA,NA,NA),
                             miscFitOptions = list(merge.small.bins = FALSE)
                             )

  var_model = fit_var$var_model
  vg = fit_var$exp_var
  var_sserr = fit_var$sserr

} else {

  vg = variogram(g)

  if(any(model %in% "Pow"))
  {
    if(Estimate_Range_and_Psill){ Range = 1; Psill = NA  }
    vgm_ = vgm(nugget = 0, psill = Psill, range = Range, model = model)
    var_model = fit.variogram(vg, model = vgm_, fit.kappa = F)

  } else if(any(model %in% "Nug")) {

    if(Estimate_Range_and_Psill){ Range = NA; Psill = NA }
    vgm_ = vgm(psill = Psill, range = Range, model = model)
    var_model = fit.variogram(object=vg, model=vgm_)

  } else {

    if(Estimate_Range_and_Psill)
    {
      Psill = max(vg$gamma) * 0.9
      Range = max(vg$dist) / 2
      Nugget = mean(vg$gamma) / 4
    }

    vgm_ = vgm(nugget = Nugget, psill = Psill, range = Range, model = model)
    var_model = fit.variogram(vg, model = vgm_, fit.kappa = TRUE)
  }
  var_sserr = attr(var_model, "SSErr")
}
VAR_DF = as.data.frame(var_model)[c("model", "psill", "range", "kappa")]
VAR_DF = round_df(VAR_DF, 4)

# AUTOMATIC RESOLUTION ====================================================
Resolution = tryCatch(abs(as.integer(Resolution)),
    warning = function(w) {
      round(sqrt((Extent[2] - Extent[1])^2 + (Extent[4] - Extent[3])^2)/500)
    })

# GRID =================================================
GRIDE = get_grid(layer = LAYER, resolution = Resolution,
                 grid.method = Grid_method, expand = Expand_vector,
                 fx = Expand_longitude, fy = Expand_latitude)

# KRIGING ==============================================
kpred = predict(g, newdata = GRIDE)

if(Local_kriging)
{
  OK = krige(frm, LAYER, newdata = kpred, model = var_model, nmax = Nearest_observations)
} else {
  OK = krige(frm, LAYER, newdata = kpred, model = var_model)
}

# RASTER ==================================================================
PRED_RASTER = raster(OK["var1.pred"])
VAR_RASTER = raster(OK["var1.var"])

# CROP AND MASK ===========================================================
if(!is.null(Mask_layer))
{
  Mask_layer = st_transform(Mask_layer, crs = CRS_Layer)
  poly_crop = st_crop(Mask_layer, Extent)
  MASK_PRED <- raster::mask(PRED_RASTER, poly_crop)

} else {

  dat1=list()
  dat1$x=seq(Extent[1], by = .1, len = 2)
  dat1$y=seq(Extent[3], by = .1, len = 2)
  dat1$z=matrix(rep(0, 4), 2, 2)

  MASK_PRED <-raster(
      dat1$z,
      xmn=range(dat1$x)[1], xmx=range(dat1$x)[2],
      ymn=range(dat1$y)[1], ymx=range(dat1$y)[2], 
      crs=crs(PRED_RASTER))
}

# OUT RASTER ==========================================
OK_variance = VAR_RASTER
OK_prediction = PRED_RASTER
OK_clip_prediction = MASK_PRED

# CROSS VALIDATION ====================================
KCV = krige.cv(frm, LAYER, var_model, nmax = Nearest_observations, nfold = Nearest_observations, verbose = FALSE)
KCV_DF = as.data.frame(KCV)

ST = get_stats(KCV)

STAT = ST[['stats']]
ST[['mean_error_res']]

# PRINT ===============================================
printInfo <- function()
{
  cat("Time:", format(Sys.time(), "%Y-%m-%d %X %Z"), "\n")
  cat("\nResolution:", Resolution,"meter\n")
}
printInfo()
# REPORT ==============================================
rp = create_report(Create_report, Open_report)

