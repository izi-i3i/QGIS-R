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
#'            : Polygon = create polygon.
#' Block_size: A vector with 1, 2 or 3 values containing the size
#'           : of a rectangular in x-, y- and z-dimension respectively (0 if
#'           : not set), or a data frame with 1, 2 or 3 columns, containing the
#'           : points that discretize the block in the x-, y- and z-dimension
#'           : to define irregular blocks relative to (0,0) or (0,0,0).
#'           : By default, predictions or simulations refer to the
#'           :  support of the data values.
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
#' Maximum: the number of nearest observations that
#'        : should be used for a kriging prediction or simulation, where
#'        : nearest is defined in terms of the space of the spatial
#'        : locations. By default, all observations are used.
#' Minimum: if the number of nearest observations within
#'        : distance ‘maxdist’ is less than ‘nmin’, a missing value will be generated.
#' N_fold: if larger than 1, then apply n-fold cross validation;
#'       : if ‘nfold’ equals ‘nrow(data)’ (the default), apply
#'       : leave-one-out cross validation; if set to e.g. 5, five-fold
#'       : cross validation is done. To specify the folds, pass an integer
#'       : vector of length ‘nrow(data)’ with fold indexes.
#' Resolution: If the value is zero it will be calculated automatically, in meters.
#' Set_Seed: Ensures that the same random values are produced every time you run the code
#'         : In blank the number generation is random.
#' Create_Report: Create report with graphs.
#' Open_Report: Open report.
#' Color_Ramp_Report: Select color palette: Spectral;Blues;Cividis;Greens;Grays;Magma;Mako;RdGy;Reds;Rocket;Turbo;Viridis;Inferno
#' Invert_Color_Ramp: Invert color ramp.
#' Draw_lines_variogram: Draw lines variogram.
#' Plot_mask: Plot layer mask.
#' Report: Directory and name of the report (docx) to be saved.
#' Plot_contour_report: Plot contour report.
#' Mask_conf_report: size = 0.3, color = 'gray60'.
#' Contour_conf_report: size = 0.5, color = 'gray50'.
#' Expand_longitude: value added to longitude.
#' Expand_latitude: value added to latitude.
#' rscripts_folder: path to rscript folder.
#' OK_variance: Kriging variance of prediction (raster)
#' OK_prediction: Kriging predicted value (raster)
#' ALG_CREATOR: <a href='https://github.com/izi-i3i/QGIS-R/'>izi-i3i</a>
#' ALG_HELP_CREATOR: izi-i3i
#' ALG_VERSION: 0.1.1

##Ordinary Kriging=name
##[QGIS-R 2025-0.1.0]=group
##QgsProcessingParameterFeatureSource|Layer|Layer vector|0|None|False
##QgsProcessingParameterCrs|CRS_Layer|CRS Layer (Planar coordenates)|EPSG:3857
##QgsProcessingParameterFeatureSource|Mask_layer|Mask Layer|2|None|True
##Field=Field Layer
##Log_Field=boolean False
##Extent=optional extent
##CRS_Extent=expression @project_crs
##qgis_version=expression @qgis_version
##proj_basename=expression @project_basename

##Grid_method=enum literal Rectangle;Convex hull;Polygon ;
##Block_size=string "0"
##Model=enum multiple Spherical (Sph);Exponential (Exp);Gaussian (Gau);Matern (Mat); Matern Stein's parameterization (Ste);Exponential class (Exc);Circular (Cir);Linear (Lin);Bessel (Bes);Pentaspherical (Pen);Periodic (Per);Wave (Wave);Hole (Hol);Logarithmic (Log);Spline (Spl);Power (Pow);Nugget (Nug)
##Auto_fit_variogram=boolean True
##Estimate_Range_and_Psill=boolean True
##Nugget=number 0
##Range=number 0
##Psill=number 0
##QgsProcessingParameterString|Maximum|Maximum (Number of nearest observations)|Inf
##QgsProcessingParameterNumber|Minimum|Minimum|QgsProcessingParameterNumber.Integer|0
##QgsProcessingParameterNumber|N_fold|N-fold cross validation|QgsProcessingParameterNumber.Integer|20
##Resolution=string "auto"
##Set_Seed=string "1234"

##Create_Report=boolean True
##Open_Report=boolean False
##QgsProcessingParameterString|title_report|Title (Report)|Ordinary Kriging Interpolation
##Color_Ramp_Report=enum literal Spectral;Blues;Cividis;Greens;Grays;Magma;Mako;RdGy;Reds;Rocket;Turbo;Viridis;Inferno ;
##QgsProcessingParameterBoolean|Invert_Color_Ramp|Invert color ramp (Report)|False
##QgsProcessingParameterBoolean|Insert_points|Insert points (Report)|True
##QgsProcessingParameterBoolean|Draw_lines_variogram|Draw lines variogram (Report)|False
##QgsProcessingParameterBoolean|Plot_mask|Plot mask (Report)|True
##QgsProcessingParameterBoolean|Plot_contour_report|Plot contour (Report)|False
##QgsProcessingParameterString|Mask_conf_report|Mask conf (Report)|size = 0.3, color = 'gray60'
##QgsProcessingParameterString|Contour_conf_report|Contour_conf (Report)|size = 0.5, color = 'gray50'
##QgsProcessingParameterNumber|Expand_longitude|Expand longitude (Report)|QgsProcessingParameterNumber.Double|0.02
##QgsProcessingParameterNumber|Expand_latitude|Expand latitude (Report)|QgsProcessingParameterNumber.Double|0.02
##QgsProcessingParameterString|min_max_pred|Minimum and maximum prediction (Report)|auto
##QgsProcessingParameterFile|rscripts_folder|Path to rscript folder|1||~/.local/share/QGIS/QGIS3|True

##Report=output file docx
##OK_variance=output raster
##OK_prediction=output raster

# ter 03 jun 2025 20:12:05
#-----------------------------------------------
# cam=1
# source("1-dados_layer_fields.R")

# ==================================================
arquivo = "ordinary_kriging.rsx"
pattern = "\\.rsx$"

# OPTIONS ==========================================
# options(scipen = 999)

# SEED =============================================
if(Set_Seed == "") Set_Seed = NULL
set.seed(Set_Seed)

# READ PACKAGES ====================================
packages = c("gstat", "sp", "sf", "automap", "raster",
             "officer", "cowplot", "ggrepel","palettes",
             "ggpmisc", "ggplot2")

for (pac in packages)
{
  if (!suppressMessages(require(pac, character.only = TRUE, quietly = TRUE)))
  {
    install.packages(pac, repos = "https://cloud.r-project.org", dependencies = TRUE)
    cat("package installed:", paste(pac, collapse = ", "),"\n")
  }
}

# DIR QGIS3 ========================================
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

  fileConn <- file(".rscript_path")
  writeLines(dir_path, fileConn)
  close(fileConn)
  novo = TRUE
} else {
  novo = FALSE
}

if (file.exists(file_path) & rscripts_folder != "")
{ # file=exists; folder=non-empty
  dir_path = rscripts_folder
  dir_path_aux = readLines(file_path)
  if (dir_path != dir_path_aux)
  { #write new path
    fileConn = file(".rscript_path")
    writeLines(dir_path, fileConn)
    close(fileConn)
  }
} else if (novo) {
    dir_path_aux = rscripts_folder
    dir_path = readLines(file_path)
  } else {
    dir_path = readLines(file_path)
    t1 = "##QgsProcessingParameterFile|rscripts_folder|Path to rscript folder|1|||True"
    list_files = list.files(dir_path, pattern = pattern, recursive = TRUE, full.names = TRUE)
    arq = grep(paste0(arquivo, collapse = "|"), list_files, value = TRUE)
    txt = readLines(arq)
    t2 = grep(t1, txt, fixed = TRUE)
    dir_path_aux = if (txt[t2[1]] == t1) "" else dir_path
}

if(!dir.exists(dir_path))
{
  stop(sprintf("\nFolder path ('%s') does not exist!\nEnter a valid path in: 'Path to rscript folder'",
               dir_path), call. = FALSE)
}

# READ FUNCTIONS ===================================
sourceFun = function(x, path)
{
  list_files = list.files(path, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  arq = grep(paste0(x, collapse = "|"), list_files, value = TRUE)
  for (i in 1:length(arq)) { source(arq[i]) }
  cat("\nLoading required function files:\n")
  cat("----------------------------------\n")
  cat(x, sep="\n")
}

fun = c("get_grid.R",
        "plot_variogram.R",
        "create_report.R",
        "change_dir.R",
        "round_df.R",
        "is_crs_planar",
        "get_stats.R",
        "printInfo.R",
        "order_magnitude.R",
        "palette_colors.R",
        "crs_txt.R")

# Loading required function
sourceFun(fun, path = dir_path)

# CHANGE DIR =======================================
change_dir(arquivo, dir_path, dir_path_aux, pattern = pattern)

# LAYER CRS-TRANSFORM ==============================
if (is_crs_planar(Layer))
{
  CRS_Layer = raster::crs(Layer)
} else {
  Layer = st_transform(Layer, crs = CRS_Layer, agr = "constant")
}

if(!is.null(Mask_layer)) Mask_layer = st_transform(Mask_layer, crs = CRS_Layer)

# EXTENT ===========================================
if(is.null(Extent))
{
  Extent = extent(st_bbox(Layer))
} else {
  xy = data.frame(x = Extent[1:2], y = Extent[3:4])
  L1 = st_as_sf(xy, coords = c("x", "y"), crs = CRS_Extent, agr = "constant")
  L2 = st_transform(L1, crs = st_crs(CRS_Layer), agr = "constant")
  Extent = extent(st_bbox(L2))
}

# MASK LAYER NULL ==================================
if (is.null(Mask_layer) & (Grid_method == "Polygon" | Grid_method == "Convex hull"))
{
  st_agr(Layer) = "constant"
  Layer = st_crop(Layer, Extent)
  Mask_layer <- st_as_sf(st_convex_hull(st_union(Layer)), crs = CRS_Layer)

  warning("Mask_layer is NULL, grid.method coerced to 'Convex hull'", call. = FALSE)
}

# LAYER ============================================
LAYER = as_Spatial(Layer)
LAYER = raster::crop(LAYER, Extent)
raster::crs(LAYER) <- raster::crs(CRS_Layer)

names(LAYER)[names(LAYER) == Field] = "Field"
LAYER = remove.duplicates(LAYER)
LAYER = LAYER[!is.na(LAYER$Field),]
if(is.character(LAYER$Field)) stop("Field must be numeric!", call. = FALSE)

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
        paste(mt_select, collapse = ", "), "',
        consider running 'Power' or 'Nugget' separately."), call. = FALSE)
  }
}

# VARIOGRAM ========================================
form = 'Field ~ 1'
frm = formula(form)
gs = gstat(id = Field, formula = frm, data = LAYER)

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

  vg = variogram(gs)

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

# TRANSFORM STRING (Block_size) INTO NUMERIC ===========
Block_size = unlist(strsplit(Block_size, ","))
Block_size = tryCatch(abs(as.integer(Block_size)), warning = function(w) {0})#NOTE: verificar

# AUTOMATIC RESOLUTION =================================
Resolution = tryCatch(abs(as.integer(Resolution)),
    warning = function(w) {
      a = (Extent[2] - Extent[1])^2
      b = (Extent[4] - Extent[3])^2
      hyp = sqrt(a + b)
      kr = if(any(Block_size > 0)) 450 else 600
      Resolution = as.integer(hyp/kr)
      if(order_magnitude(hyp)[2] > 100000) Resolution = as.integer(Resolution * 1.1)
      if(order_magnitude(hyp)[2] > 1000000) Resolution = as.integer(Resolution * 2.0)
      Resolution
    })

# GRID =================================================
GRIDE = get_grid(LAYER,
                 extent = Extent,
                 mask.layer = Mask_layer,
                 resolution = Resolution,
                 grid.method = Grid_method,
                 fx = 0.01,
                 fy = 0.01)

# TRANSFORM STRING (Maximum) INTO NUMERIC ==============
Maximum = tryCatch(abs(as.numeric(Maximum)), warning = function(w) { Inf })

# PREDICT ==============================================
# kpred = predict(gs, newdata = GRIDE, block = Block_size)#NOTE: verificar

# KRIGING ==============================================
OK = krige(frm, LAYER, newdata = GRIDE, model = var_model, nmax = Maximum, nmin = Minimum, block = Block_size)

# RASTER ==================================================================
PRED_RASTER = raster(OK[1])
VAR_RASTER = raster(OK[2])

# CROP AND MASK ===========================================================
if (!is.null(Mask_layer))
{
  st_agr(Mask_layer) = "constant"
  Mask_layer = st_crop(Mask_layer, Extent)
  PRED_RASTER = raster::mask(PRED_RASTER, Mask_layer)
  VAR_RASTER = raster::mask(VAR_RASTER, Mask_layer)
}

# OUT RASTER ==========================================
OK_variance = VAR_RASTER
OK_prediction = PRED_RASTER

# INFO ================================================
cat("\nConfig\n")
cat("----------------------------------\n")
printInfo()

# REPORT ==============================================
title_report = paste("Kriging Interpolation", "-", proj_basename)
rp = create_report(tit = title_report,
                   LAYER,
                   PRED_RASTER,
                   VAR_RASTER,
                   mask.layer = Mask_layer,
                   var.model = var_model,
                   n.fold = N_fold,
                   create.report = Create_Report,
                   open.report = Open_Report,
                   plot.mask = Plot_mask,
                   plot.contour = Plot_contour_report,
                   mask.conf = Mask_conf_report,
                   contour.conf = Contour_conf_report,
                   color.ramp = palette_colors(name = Color_Ramp_Report, n = 100, reverse = Invert_Color_Ramp),
                   fx = Expand_longitude,
                   fy = Expand_latitude
                   )
# ---------------------------------
