#' ALG_DESC: <p>This file creates a <span style='text-decoration: underline;'>Universal Kriging</span>.
#'         : This script does Ordinary Kriging interpolation from a numeric field of a points vector layer.
#'         : It allows to auto select the initial values for nugget, psill and range; or it can fit a model
#'         : from initial values provided. Besides, you can limit the number of points used to predict.</p>
#' Layer: points vector layer.
#' CRS_Layer: Planar coordinates.
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
#' Local_kriging: If checked, points to interpolate will be limited to a number of nearest observations.
#' Number_of_nearest_observations: Maximun number of observations used in local kriging.
#' Estimate_Range_and_Psill: If checked, initial values for nugget, psill and range will be
#'                         : estimated from sample variogram.
#' Nugget: Iniital value for nugget
#' Psill: Initial value for partial sill
#' Range: Initial value for range
#' Resolution: If the value is zero it will be calculated automatically, in meters.
#' Color_report: Select color palette: Turbo, Magma, Inferno, Plasma, viridis, Cividis, Rocket, Mako.
#' Create_report: Create report with graphs.
#' Open_report: Open report.
#' Report: Directory and name of the report (docx) to be saved.
#' UK_variance: Kriging variance of prediction (raster)
#' UK_prediction: Kriging predicted value (raster)
#' ALG_CREATOR: <a href='https://github.com/izi-i3i/QGIS-R/'>izi-i3i</a>
#' ALG_HELP_CREATOR: izi-i3i
#' ALG_VERSION: 0.0.1

##Universal Kriging=name
##[R-Geostatistics]=group
##Layer=vector
##QgsProcessingParameterCrs|CRS_Layer|CRS Layer (meter)|EPSG:3395
##Field=Field Layer
##Log_Field=boolean False
##Extent=extent
##CRS_Extent=expression @project_crs
##Grid_method=enum literal Rectangle;Convex hull ;
##Expand_vector=boolean True
##QgsProcessingParameterNumber|Expand_longitude|Expand longitude (only rectangle)|QgsProcessingParameterNumber.Double|0.01
##QgsProcessingParameterNumber|Expand_latitude|Expand latitude (only rectangle)|QgsProcessingParameterNumber.Double|0.01
##Model=enum multiple Spherical (Sph);Exponential (Exp);Gaussian (Gau);Matern (Mat); Matern Stein's parameterization (Ste);Exponential class (Exc);Circular (Cir);Linear (Lin);Bessel (Bes);Pentaspherical (Pen);Periodic (Per);Wave (Wave);Hole (Hol);Logarithmic (Log);Spline (Spl);Power (Pow);Nugget (Nug)
##Estimate_Range_and_Psill=boolean True
##Nugget=number 0
##Range=number 0
##Psill=number 0
##Local_kriging=boolean False
##QgsProcessingParameterNumber|Nearest_observations|Number of nearest observations|QgsProcessingParameterNumber.Integer|25
##QgsProcessingParameterNumber|Resolution|Resolution (meter)|QgsProcessingParameterNumber.Integer|0
##Set_Seed=boolean True
##QgsProcessingParameterNumber|Seed|Number Seed|QgsProcessingParameterNumber.Integer|1234
##Color_report=enum literal Turbo;Magma;Inferno;Plasma;Viridis;Cividis;Rocket;Mako ;
##Create_report=boolean True
##Insert_points=boolean True
##Draw_lines_variogram=boolean True
##Report=output file docx
##Open_report=boolean False
##UK_variance=output raster
##UK_prediction=output raster


# PROCESSING TIME =================================
tictoc::tic()

# READ PACKAGES ====================================
packages = c("gstat", "sp", "sf", "automap", "raster", "ggrepel",
             "officer", "cowplot", "viridis", "ggpmisc", "ggplot2")

for (pac in packages) {
  if (!suppressMessages(require(pac, character.only=TRUE, quietly=TRUE))) {
    install.packages(pac, repos = getOption("repos"), dependencies=TRUE)
    cat("package installed:", paste(pac, collapse = ", "),"\n")
  }
}

# READ FUNCTIONS ==========================================================
sourceFun = function(fun, trace = FALSE, ...)
{
  os <- .Platform$OS.type
  if(os == "unix"){
    dir_fl = "~/.local/share/QGIS/QGIS3"
  } else if(os == "windows"){
    dir_fl = "~\\AppData\\Roaming\\QGIS\\QGIS3"
  } else if(os == "osx"){
    dir_fl = "~/Library/Application\\ Support/QGIS/QGIS3"
  }

  fl = list.files(dir_fl, recursive = T, full.names = T)
  arq = grep(paste0(fun, collapse="|"), fl, value = T)
  for (i in 1:length(arq)) {
    if(trace) cat(i,":", arq[i], "\n", sep="")
    source(arq[i], ...)
  }#end for
}

cat("\n ----------------------------------\n")
cat("Loading required functions:\n")
fun = c("get_grid.R", "plot_variogram.R", "create_report.R")
sourceFun(fun, trace=T)
cat(" ----------------------------------\n")

# =========================================================================
if(Set_Seed) set.seed(Seed)
Color_report = tolower(Color_report)

# LAYER TRANSFORM =================================
Layer = st_transform(Layer, crs = CRS_Layer)

# INFO ============================================
# scientific notation
options(scipen = 9999)
# extract crs
crs_info = st_crs(Layer)
crs_unit = st_crs(crs_info, parameters = TRUE)$units_gdal
crs_num = crs_info$epsg
epsg_crs_txt = paste0("EPSG: ", crs_num, " CRS: ", st_crs(st_sfc(crs = crs_num))$Name)

# CRS ==============================================
ex = data.frame(x=Extent[1:2], y=Extent[3:4])
L1 = st_as_sf(ex, coords = c("x", "y"), crs = CRS_Extent, agr = "constant")
L2 = st_transform(L1, crs = CRS_Layer)
Extent = st_bbox(L2)[c('xmin', 'xmax', 'ymin', 'ymax')]

# LAYER ============================================
LAYER = as_Spatial(Layer)
LAYER = crop(LAYER, Extent)

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
f = c('Field~1', 'Field~x+y', 'Field~dist')[2]
frm = formula(f)
g = gstat(id = Field, formula = frm, data = LAYER)
vg = variogram(g)

if(F){ #NOTE: Decidir qual o melhor

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

  if(Estimate_Range_and_Psill) {
    Psill = max(vg$gamma)*0.9
    Range = max(vg$dist)/2
    Nugget = mean(vg$gamma)/4
  }
  vgm_ = vgm(nugget = Nugget, psill = Psill, range = Range, model = model)
#   vgm_ = vgm(nugget = NA, psill = NA, range = NA, model = model)

  var_model = fit.variogram(vg, model = vgm_, fit.kappa = T)
}

} else {#NOTE: Decidir qual o melhor

fit_var = autofitVariogram(frm,
                             LAYER,
                             model = model,
                             kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10), 
                             fix.values = c(NA,NA,NA),
                             verbose = F,
                             GLS.model = NA,
                             start_vals = c(NA,NA,NA),
                             miscFitOptions = list())

var_model = fit_var$var_model
exp_var = fit_var$exp_var
var_sserr = fit_var$sserr
}

MDF = as.data.frame(var_model)[c("model", "psill", "range", "kappa")]

if(Create_report)
{
  pv1 = plot_variogram(vg, var_model, Model)
  pv2 = ggplot() +
    theme_void() +
    annotate(geom='table', x=1, y=1, label=list(MDF), size=4)
  p1 = cowplot::plot_grid(pv1, pv2, nrow = 2, ncol = 1, rel_heights = c(6, 1) ) +
    theme(plot.background = element_rect(fill = "white", colour = NA))
}

# AUTOMATIC RESOLUTION ====================================================
if(Resolution <= 0) {
  Resolution = round(sqrt((Extent[2] - Extent[1])^2 + (Extent[4] - Extent[3])^2)/200)
}

# GRID =================================================
GRIDE = get_grid(layer = LAYER, resolution = Resolution,
                 grid.method = Grid_method, expand = Expand_vector,
                 fx = Expand_longitude, fy = Expand_latitude)

# KRIGING ==============================================
kpred = predict(g, newdata = GRIDE)

if(Local_kriging)
{
  UK = krige(frm, locations=LAYER, newdata = kpred, var_model, nmax = Nearest_observations, block = c(40,40))
} else {
  UK = krige(frm, locations=LAYER, newdata = kpred, var_model, block = c(40,40))
}

# PLOT KRIGING =========================================
PRED_RASTER = raster(UK)
PRED_RASTER_DF = as.data.frame(PRED_RASTER, xy = TRUE)
LAYER_DF = as.data.frame(LAYER)

if(Create_report)
{
  p2 = ggplot() +
    theme_bw() +
    geom_raster(data = PRED_RASTER_DF , aes(x = x, y = y, fill = var1.pred)) +
    ifelse(Insert_points, list(geom_point(data=LAYER_DF, aes(x = x, y = y), shape=20)), list(NULL)) +
    scale_fill_viridis(option = Color_report, name=Field, na.value="transparent") +
    scale_y_continuous(expand = expansion(mult=0.01)) +
    scale_x_continuous(expand = expansion(mult=0.01)) +
    coord_fixed(expand = TRUE, clip = "off") +
    theme(axis.text.y = element_text(angle=90, hjust=.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(x="longitude", y="latitude", caption = epsg_crs_txt)
}

# PLOT VARIOGRAM ========================================
VAR_RASTER = raster(UK["var1.var"])
VAR_RASTER_DF = as.data.frame(VAR_RASTER, xy = TRUE)

if(Create_report)
{
  p3 = ggplot() +
    theme_bw() +
    geom_raster(data = VAR_RASTER_DF, aes(x = x, y = y, fill = var1.var)) +
    ifelse(Insert_points, list(geom_point(data=LAYER_DF, aes(x = x, y = y), shape=20)), list(NULL)) +
    scale_fill_viridis(option = Color_report, name=Field, na.value="transparent") +
    scale_y_continuous(expand = expansion(mult=0.01)) +
    scale_x_continuous(expand = expansion(mult=0.01)) +
    coord_fixed(expand = TRUE, clip = "off") +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(x="longitude", y="latitude", caption = epsg_crs_txt)
}

# =========================================================================
if(Create_report)
{
  # variogram calculation, cloud=TRUE is for cloud scatter 
  meuse.varioc <- variogram(frm, LAYER, cloud=TRUE)

  # Customizing the cloud plot
  p4 = ggplot(meuse.varioc, aes(x=meuse.varioc$dist, y=meuse.varioc$gamma)) + 
    geom_point(color = "blue", fill = "blue", size = 2, alpha = 0.25) +
    scale_x_continuous(expand = expansion(mult=0.03)) +
    scale_y_continuous(expand = expansion(mult=0.01)) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#           axis.text.y = element_text(angle = 90, hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(title = NULL,
         x = "h(m)",
         y = bquote(gamma~"(h)"))
}

# CROSS VALIDATION =====================================
KCV = krige.cv(frm, LAYER, var_model, nmax = 25, nfold = 5, verbose = FALSE)
KCV_DF = as.data.frame(KCV)
# sserr = attr(var_model, "SSErr")
# Mean error, ideally 0:
mean_error_res = mean(KCV$residual)
# Mean Square Prediction Error, ideally small
mspe = mean(KCV$residual^2)
# Mean square normalized error, ideally close to 1
mean_z2 = mean(KCV$zscore^2)
# Correlation observed and predicted, ideally 1
cor_obs_pred = cor(KCV$observed, KCV$var1.pred)
# Correlation predicted and residual, ideally 0
cor_pred_red = cor(KCV$var1.pred, KCV$residual)

STAT = data.frame(Stat = c("Sum of Squares Error (SSE)",
                           "Mean error residual",
                           "Mean Square Prediction Error (MSPE)",
                           "Mean square normalized error",
                           "Correlation observed and predicted",
                           "Correlation predicted and residual"),
                  Observed = c(var_sserr, mean_error_res, mspe, mean_z2, cor_obs_pred, cor_pred_red),
                  Ideally = c(0, 0, "small", "close to 1", 1, 0))

# PRINT ===============================================
printInfo <- function()
{
  cat("Model:","\n")
  print(MDF, row.names = T,right = T)
  cat("\nResolution:", Resolution,"meter\n")
  cat("\nStatistical:\n")
  cat("SSE:", attr(var_model, "SSErr"),"\n")
  cat("Mean error residual:", mean_error_res, "\n")
  cat("MSPE:", mspe, "\n")
  cat("Mean square normalized error:", mean_z2, "\n")
  cat("Correlation observed and predicted:", cor_obs_pred, "\n")
  cat("Correlation predicted and residual:", cor_pred_red, "\n")
}
printInfo()

# REPORT ==============================================
create_report(Create_report, Open_report)

# OUT =================================================
UK_variance = VAR_RASTER
UK_prediction = PRED_RASTER

# ======================================================
nome_doc = paste("kriging on the field vector:", Field)
msg_toc = function(tic,toc,msg,arq){
  paste(" ----------------------------------\n",
        arq, "\n",
        "R algorithm completed in",round(toc - tic, 3), "seconds\n",
        "Report created on",Report,"\n",
        "----------------------------------\n")
}
tictoc::toc(func.toc = msg_toc, arq=nome_doc)
