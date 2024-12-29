#' ALG_DESC: <p>This file creates a <span style='text-decoration: underline;'>Ordinary Kriging</span>.
#'         : This script does Ordinary Kriging interpolation from a numeric field of a points vector layer.
#'         : It allows to auto select the initial values for nugget, psill and range; or it can fit a model
#'         : from initial values provided. Besides, you can limit the number of points used to predict.</p>
#' Layer: points vector layer.
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
#' Color_plots: Select color palette: turbo, magma, inferno, plasma, viridis, cividis, rocket, mako
#' Create_report: Create report with graphs.
#' Open_report: Open report.
#' Report: Directory and name of the report (docx) to be saved.
#' Kriging_variance: Kriging variance of prediction (raster)
#' Kriging_prediction: Kriging predicted value (raster)
#' ALG_CREATOR: <a href='https://github.com/izi-i3i/QGIS-R/'>izi-i3i</a>
#' ALG_HELP_CREATOR: izi-i3i
#' ALG_VERSION: 0.0.3

##Ordinary Kriging=name
##[R-Geostatistics]=group
##Layer=vector
##Field=Field Layer
##Log_Field=boolean False
##Extent=extent
##Grid_method=enum literal Rectangle; Convex hull ;
##Expand_vector=boolean True
##QgsProcessingParameterNumber|Expand_longitude|Expand longitude (only rectangle)|QgsProcessingParameterNumber.Double|0.01
##QgsProcessingParameterNumber|Expand_latitude|Expand latitude (only rectangle)|QgsProcessingParameterNumber.Double|0.01
##Model=enum multiple Spherical (Sph); Exponential (Exp); Gaussian (Gau); Matern (Mat); Matern Stein's parameterization (Ste); Exponential class (Exc); Circular (Cir); Linear (Lin); Bessel (Bes); Pentaspherical (Pen);  Periodic (Per); Wave (Wave); Hole (Hol); Logarithmic (Log); Spline (Spl); Power (Pow); Nugget (Nug)
##Estimate_Range_and_Psill=boolean True
##Nugget=number 0
##Range=number 0
##Psill=number 0
##Local_kriging=boolean False
##QgsProcessingParameterNumber|nearest_observations|Number of nearest observations|QgsProcessingParameterNumber.Integer|25
##QgsProcessingParameterNumber|Resolution|Resolution (meter)|QgsProcessingParameterNumber.Integer|0
##Color_plots=enum literal turbo;magma;inferno;plasma;viridis;cividis;rocket;mako ;
##Create_report=boolean True
##Insert_points=boolean True
##Draw_lines_variogram=boolean True
##Report=output file docx
##Open_report=boolean False
##Kriging_variance=output raster
##Kriging_prediction=output raster

# READ PACKAGES ====================================
packages = c("gstat", "ggpmisc", "ggplot2", "sp", "sf", "raster","officer",
             "tictoc", "cowplot", "viridis", "ggrepel", "latex2exp")
for (pac in packages) {
  if (!suppressMessages(require(pac, character.only=TRUE, quietly=TRUE))) {
    install.packages(pac, repos = getOption("repos"), dependencies=TRUE)
    cat("package installed:", paste(pac, collapse = ", "),"\n")
  }
}

# PROCESSING TIME =================================
tic()

# INFO ============================================
# scientific notation
options(scipen = 10000)
# extract crs
crs_info = sf::st_crs(Layer)
epsg_crs = paste0("EPSG: ", crs_info$epsg, " CRS: ",st_crs(st_sfc(crs = crs_info$epsg))$Name)

# =================================================
plot_variogram = function(vg, fit.vgm, model = NULL) {
  preds = variogramLine(fit.vgm, maxdist = max(vg$dist))
  breaks = pretty(range(vg$np), n = nclass.Sturges(vg$np)-1, min.n = 1)

  MDF = as.data.frame(fit.vgm)[c("psill", "range")]

  if(length(fit.vgm$model) < 2)
  {
    nrp_line = list(
      geom_hline(yintercept = max(MDF$psill), linetype = 2, color = "gray55"),
      geom_vline(xintercept = max(MDF$range), linetype = 2, color = "gray55"),
      geom_segment(data=MDF, aes(x = 0, y = 0, xend = range, yend = 0, color="Range"),
                   linetype = 1, arrow = arrow(length = unit(0.15,"cm"), ends="both")),
      geom_segment(data=MDF, aes(x = max(range), y=0, xend=max(range), yend=max(psill), color="Psill"),
                   linetype = 1, arrow = arrow(length = unit(0.15,"cm"), ends="both"))
    )
  } else {
    L1 = data.frame(x = 0, y = 0, xend = 0, yend = min(MDF$psill))
    L2 = data.frame(x = 0, y = min(MDF$psill), xend = max(MDF$range), yend = min(MDF$psill))
    L3 = data.frame(x = max(MDF$range), y = min(MDF$psill), xend = max(MDF$range), yend = max(MDF$psill))

    nrp_line = list(
      geom_hline(yintercept=max(MDF$psill), linetype=2, color="gray55"),
      geom_vline(xintercept=max(MDF$range), linetype=2, color="gray55"),
      geom_segment(data=L1, aes(x = x, y = y, xend = xend, yend = yend, color="Nugget"),
                   linetype = 1, arrow = arrow(length = unit(0.15,"cm"), ends="both")),
      geom_segment(data=L2, aes(x = x, y = y, xend = xend, yend = yend, color="Range"),
                   linetype = 1, arrow = arrow(length = unit(0.15,"cm"), ends="both")),
      geom_segment(data=L3, aes(x = x, y = y, xend = xend, yend = yend, color="PSill"),
                   linetype = 1, arrow = arrow(length = unit(0.15,"cm"), ends="both"))
    )
  }

  colores = c("#CDB79E", "#A52A2A", "#FF8C00", "#008B00", "black")
  leg_colors = if(length(fit.vgm$model) < 2) colores[c(1,3,4,5)] else colores

  g1 = ggplot(data = vg, aes(x = dist, y = gamma)) +
    theme_bw() +
    geom_line(data = preds, aes(x = dist, y = gamma, color="Theoretical"), linewidth = 1.0, inherit.aes = FALSE) +
    geom_line(aes(color = "Experimental"), linetype = 1) +
    ifelse(Draw_lines_variogram, list(nrp_line), list(NULL)) +
    geom_point(aes(size=np, fill = np), shape = 21) +
    scale_color_manual(values = leg_colors) +
    geom_text_repel(aes(label = np)) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_size_binned(range = c(1, 10), breaks = breaks, name = "n point pairs") +
    scale_fill_viridis(option = Color_plots) +
    guides(fill = guide_legend(order=0), size = guide_legend(order=0), color = guide_legend()) +
    labs(x = "distance", y = "semivariance", fill = "n point pairs", color = "")

    return(g1)
}

# =================================================
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
  gridded(gride) = TRUE
  attr(gride, "proj4string") = layer@proj4string
  return(gride)
}

# =================================================
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
} else {
  Field = NULL
}

# VARIOGRAM ========================================
model = c("Sph", "Exp", "Gau",
          "Mat", "Ste", "Exc",
          "Cir", "Lin", "Bes",
          "Pen", "Per", "Wav",
          "Hol", "Log", "Spl",
          "Pow", "Nug")[sort(Model+1)]

model_type = c("Spherical (Sph)", 
               "Exponential (Exp)",
               "Gaussian (Gau)",
               "Stein's parameterization (Ste)",
               "Exponential class (Exc)",
               "Matern (Mat)",
               "Circular (Cir)",
               "Linear (Lin)",
               "Bessel (Bes)",
               "Pentaspherical (Pen)",
               "Periodic (Per)",
               "Wave (Wave)",
               "Hole (Hol)",
               "Logarithmic (Log)",
               "Spline (Spl)",
               "Power (Pow)",
               "Nugget (Nug)")
names(model_type) <- model
mt_select = model_type[sort(Model+1)]

pn = model %in% c("Pow", "Nug")
ln = length(model)

if(ln > 15 | sum(pn) > 0)
{
  if(ln < 15 & any(pn) & length(pn) < 3)
  {
    model = model[1]
    md = md[1]
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

g = gstat(id = Field, formula = Field~1, data = LAYER)
vg = variogram(g)

if(any(model %in% "Pow"))
{
  if(Estimate_Range_and_Psill){ Range = 1 }
  if(Estimate_Range_and_Psill){ Psill = NA }
  vgm_ = vgm(nugget = Nugget, psill = Psill, range = Range, model = model)
  fit_vgm = fit.variogram(vg, model = vgm_, fit.kappa = F)
} else if(any(model %in% "Nug")) {
  if(Estimate_Range_and_Psill){ Range = NA }
  if(Estimate_Range_and_Psill){ Psill = NA }
  vgm_ = vgm(pasill=NA, range = NA, model = model)
  fit_vgm = fit.variogram(object=vg, model=vgm_)
} else {
  if(Estimate_Range_and_Psill){ Range = NA }
  if(Estimate_Range_and_Psill){ Psill = NA }
  vgm_ = vgm(nugget = Nugget, psill = Psill, range = Range, model = model)
  fit_vgm = fit.variogram(vg, model = vgm_, fit.kappa = F)
}

MDF = as.data.frame(fit_vgm)[c("model", "psill", "range", "kappa")]
if(Create_report)
{
  pv1 = plot_variogram(vg, fit_vgm, Model)
  pv2 = ggplot() +
    theme_void() +
    annotate(geom='table', x=1, y=1, label=list(MDF), size=4)
  p1 = cowplot::plot_grid(pv1, pv2, nrow = 2, ncol = 1, rel_heights = c(6, 1) ) +
    theme(plot.background = element_rect(fill = "white", colour = NA))
}

# Automatic resolution
# =========================================================================
if(Resolution <= 0) {
  Resolution = round(sqrt((Extent[2] - Extent[1])^2 + (Extent[4] - Extent[3])^2)/400)
}

# GRID =================================================
GRIDE = get_grid(layer = LAYER, resolution = Resolution,
                 grid.method = Grid_method, expand = Expand_vector,
                 fx = Expand_longitude, fy = Expand_latitude)

# KRIGING ==============================================
kpred = predict(g, newdata = GRIDE)

if(Local_kriging)
{
  OK = krige(Field~1, LAYER, newdata = kpred, fit_vgm, nmax = nearest_observations)
} else {
  OK = krige(Field~1, LAYER, newdata = kpred, fit_vgm)
}

# PLOT KRIGING =========================================
PRED_RASTER = raster(OK)
PRED_RASTER_DF = as.data.frame(PRED_RASTER, xy = TRUE)
LAYER_DF = as.data.frame(LAYER)

if(Create_report)
{
  p2 = ggplot() +
    theme_bw() +
    geom_raster(data = PRED_RASTER_DF , aes(x = x, y = y, fill = var1.pred)) +
    ifelse(Insert_points, list(geom_point(data=LAYER_DF, aes(x = x, y = y))), list(NULL)) +
    scale_fill_viridis(option = Color_plots, name=Field, na.value="transparent") +
    scale_y_continuous(expand = expansion(mult=0.01)) +
    scale_x_continuous(expand = expansion(mult=0.01)) +
    coord_fixed(expand = TRUE, clip = "off") +
    theme(axis.text.y = element_text(angle=90, hjust=.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(x="longitude", y="latitude", caption = epsg_crs)
}

# PLOT VARIOGRAM ========================================
VAR_RASTER = raster(OK["var1.var"])
VAR_RASTER_DF = as.data.frame(VAR_RASTER, xy = TRUE)

if(Create_report)
{
  p3 = ggplot() +
    theme_bw() +
    geom_raster(data = VAR_RASTER_DF, aes(x = x, y = y, fill = var1.var)) +
    ifelse(Insert_points, list(geom_point(data=LAYER_DF, aes(x = x, y = y))), list(NULL)) +
    scale_fill_viridis(option = Color_plots, name=Field, na.value="transparent") +
    scale_y_continuous(expand = expansion(mult=0.01)) +
    scale_x_continuous(expand = expansion(mult=0.01)) +
    coord_fixed(expand = TRUE, clip = "off") +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(x="longitude", y="latitude", caption = epsg_crs)
}

# OUT =================================================
Kriging_variance = VAR_RASTER
Kriging_prediction = PRED_RASTER

# CROSS VALIDATION =====================================
KCV = krige.cv(Field~1, LAYER, fit_vgm, nmax = 25, nfold = 5, verbose = FALSE)
KCV_DF = as.data.frame(KCV)
sse = attr(fit_vgm, "SSErr")
# Mean error, ideally 0:
mean_error_res = mean(KCV$residual)
# MSPE, ideally small
mspe = mean(KCV$residual^2)
# Mean square normalized error, ideally close to 1
mean_z2 = mean(KCV$zscore^2)
# Correlation observed and predicted, ideally 1
cor_obs_pred = cor(KCV$observed, KCV$var1.pred)
# Correlation predicted and residual, ideally 0
cor_pred_red = cor(KCV$var1.pred, KCV$residual)

STAT = data.frame(Stat = c("Sum of Squares Error (SSR)",
                           "Mean error residual",
                           "Mean Square Prediction Error (MSPE)",
                           "Mean square normalized error",
                           "Correlation observed and predicted",
                           "Correlation predicted and residual"),
                  Observed = c(sse, mean_error_res, mspe, mean_z2, cor_obs_pred, cor_pred_red),
                  Ideally = c(0, 0, "small", "close to 1", 1, 0))

# PRINT ===============================================
print.info <- function()
{
  cat("Model:","\n")
  print(MDF, row.names = T)
  cat("\nResolution:", Resolution,"meter\n")
  cat("\nStatistical:\n")
  cat("SSE:", attr(fit_vgm, "SSErr"),"\n")
  cat("Mean error residual:", mean_error_res, "\n")
  cat("MSPE:", mspe, "\n")
  cat("Mean square normalized error:", mean_z2, "\n")
  cat("Correlation observed and predicted:", cor_obs_pred, "\n")
  cat("Correlation predicted and residual:", cor_pred_red, "\n")
}
print.info()

# REPORT ==============================================
if(Create_report)
{
  # Calculating the Sturges bins
  breaks = pretty(range(KCV$residual), n = nclass.Sturges(KCV$residual), min.n = 1)

  a1 = ggplot(KCV_DF, aes(x = residual)) +
    theme_bw(12) +
    geom_histogram(color = "black", fill = "azure3", breaks = breaks) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult=c(0,0.1))) +
    scale_x_continuous() +#(breaks = breaks) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 12)) +
    geom_vline(xintercept = mean_error_res, color="red", linetype = 2) +
    labs(x="Residual", y="Count", title="Histogram")

  a2 = ggplot(KCV_DF, aes(x=observed, y=var1.pred)) +
    theme_bw(12) +
    stat_poly_line(formula = y ~ x, color = "blue", se=F) +
    stat_poly_eq(formula = y ~ x, mapping = use_label("R2", "eq"), color = "blue") +
    stat_poly_line(formula = y ~ x-1, color = "red", se=F, linetype=2) +
    stat_poly_eq(formula = y ~ x-1,mapping = use_label("R2", "eq"), color = "red", label.y = 0.88) +
    geom_point() +
    theme(panel.grid = element_blank(), plot.title = element_text(size = 12)) +
    labs(x="Obseved", y="Predicted", title="Cross Validation")

  a3 = ggplot(KCV_DF, aes(x = x, y = y, z = residual)) +
    theme_bw(12) +
    geom_point(aes(fill = residual, size = residual), alpha=.5, shape=21) +
    scale_size_area(max_size = 8) +
    scale_fill_viridis(option = Color_plots) +
    guides(fill = guide_legend(), size = guide_legend()) +
    theme(plot.title = element_text(size = 12),
          legend.title = element_blank(),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()) +
    labs(x=NULL, y=NULL, title="Bubble Residuals")

  a4 = ggplot(KCV_DF, aes(sample = residual)) +
    theme_bw() +
    stat_qq(distribution = qnorm) +
    stat_qq_line(line.p = c(.25, .75)) +
    labs(x="Theoretical Quantiles", y="Residual Quantiles", title="Q-Q Plot")

  g1 = plot_grid(a1,a2,a3,a4)

  title_ = fpar(ftext("Ordinary Kriging Interpolation", prop = shortcuts$fp_bold(font.size = 15)))
  txt_open = fpar(run_linebreak(), "")
  txt_crossvalidation = fpar("", run_linebreak())
  txt_variogram = fpar("", run_linebreak())
  txt_kriging = fpar("",run_linebreak())
  txt_variance = fpar("",run_linebreak())

  mdf_model = as.character(MDF$model)

  if(length(MDF$model) > 1)
  {
    model_fit = model_type[mdf_model[2]]
  } else {
    model_fit = model_type[mdf_model]
  }

  cap_table_model = if(ln > 1)
  {
    paste("The best fit returned was", model_fit,
      "of the selected models: ", paste(mt_select, collapse = ", "), ".")
  } else {
    paste("The fit returned", model_fit, "model.")
  }
  cap_table_stat = "Accuracy of kriging results."

  cap_fig_g1 = paste("Leave-One-Out Cross-Validation (LOOCV) for the",
                     model_fit, "model validation.")
  cap_fig_p1 = paste(model_fit,"model fitted to residual values from the first-order trend surface.")
  cap_fig_p2 = paste("Final result of interpolation, the prediction map.")
  cap_fig_p3 = "Final result of interpolation, the variance or error map."

  run_num = run_autonum(seq_id = "fig", pre_label = "Figure ", bkm = "figure")
  run_num_table = run_autonum(seq_id = "tab", pre_label = "Table ", bkm = "table")

  doc = read_docx() |>
    body_add(title_) |>
    body_add(value = txt_open, style = "Normal") |>
    body_add_par(value = "Stats", style = "heading 1") |>
    body_add_par(value = run_linebreak(), style = "Normal") |>
    body_add_caption(value = block_caption(cap_table_model, style = "Normal", autonum = run_num_table)) |>
    body_add_table(MDF, style = "Table Professional") |>
    body_add_par(value = run_linebreak(), style = "Normal") |>
    body_add_caption(value = block_caption(cap_table_stat, style = "Normal", autonum = run_num_table)) |>
    body_add_table(STAT, style = "Table Professional") |>

    body_add_break() |>

    body_add_par(value = "Cross Validation", style = "heading 1") |>
    body_add(value = txt_crossvalidation, style = "Normal") |>
    body_add_gg(value = g1, width = 6.3, height = 6.4, scale= .8, res=150) |>
    body_add_caption(value = block_caption(cap_fig_g1, style = "Normal", autonum = run_num)) |>

    body_add_break() |>

    body_add_par(value = "Variogram", style = "heading 1") |>
    body_add(value = txt_variogram, style = "Normal") |>
    body_add_gg(value = p1, width = 6.3, height = 6.5, res=150) |>
    body_add_caption(value = block_caption(cap_fig_p1, style = "Normal", autonum = run_num)) |>

    body_add_break() |>

    body_add_par(value = "Ordinary Kriging simulation", style = "heading 1") |>
    body_add(value = txt_kriging, style = "Normal") |>
    body_add_gg(value = p2, width = 6.3, height = 6.3, res=150) |>
    body_add_caption(value = block_caption(cap_fig_p2, style = "Normal", autonum = run_num)) |>

    body_add_break() |>

    body_add_par(value = "Ordinary Kriging variance", style = "heading 1") |>
    body_add(value = txt_variance, style = "Normal") |>
    body_add_gg(value = p3, width = 6.3, height = 6.3, res=150) |>
    body_add_caption(value = block_caption(cap_fig_p3, style = "Normal", autonum = run_num)) |>

    body_add_break() |>

    body_add_par(value = "Session Info", style = "heading 1") |>
    body_add(value = capture.output(sessionInfo()), style = "Normal")

  print(doc, target = Report)
  if(Open_report) browseURL(Report)
}

  nome_doc = paste("kriging on the field vector:", Field)
  msg_toc = function(tic,toc,msg,arq){
    paste(" ----------------------------------\n",
          arq, "\n",
          "R algorithm completed in",round(toc - tic, 3), "seconds\n",
          "Report created on",Report,"\n",
          "----------------------------------\n")
}
# ======================================================
toc(func.toc = msg_toc, arq=nome_doc)
