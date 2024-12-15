#' ALG_DESC: <p>This file creates a <span style='text-decoration: underline;'>Ordinary Kriging</span>.
#'         : This script does Ordinary Kriging interpolation from a numeric field of a points vector layer.
#'         : It allows to auto select the initial values for nugget, psill and range; or it can fit a model
#'         : from initial values provided. Besides, you can limit the number of points used to predict.</p>

#' Layer: points vector layer.
#' Field: numeric field from layer to interpolate.
#' Extent: Specifies a numeric variable of length 4 values (xmin, xmax, ymin and ymax).
#' Grid_method: Method to calculate the extent of interpolation.
#'            : extent vectot and ConvexHull = create a shape is the smallest convex set that contains it.

#' Expand_vector: <p>Expands longitude and latitude values by 0.01 (default)<br/>
#'              : Expand longitude: value added to longitude.<br/>
#'              : Expand latitude: value added to latitude</p>

#' Model: Model type, Spherical, Exponential, Gaussian and Stein's parameterization.
#'      : Can be a character vector of model types combined, e.g. [x] Spherical [x] Exponential, in which case the best fitting is returned.

#' Local_kriging: Boolean. If checked, points to interpolate will be limited to a number of nearest observations,
#' Number_of_nearest_observations: Maximun number of observations used in local kriging,
#' Estimate_range_and_psill_initial_values_from_sample_variogram: Boolean. If checked, initial values for nugget, psill and range will be estimated from sample variogram.
#' Nugget: Iniital value for nugget
#' Psill: Initial value for partial sill
#' Range: Initial value for range
#' Show_Sum_of_Square_Errors: Boolean. If checked, it will show the sum of squared errors of the model fitting in R console output.
#' Log10_field_kriging: log10(Field)~1
#' Resolution: Cellsize of the interpolation raster, in meters.  Only for projected layers. Layers in lat-long will be interpolated over 5000 meter.
#' Kriging_variance: Kriging variance of prediction
#' Kriging_prediction: Kriging predicted value
#' RPLOTS: <b style='text-decoration: underline;'>Output path</b> for html file with the scatterplot
#' ALG_CREATOR: <a href=''>izi31416@protonmail.com</a>
#' ALG_HELP_CREATOR: izi31416@protonmail.com
#' ALG_VERSION: 0.0.1

##Ordinary Kriging=name
##[R-Geostatistics]=group
##Layer=vector
##Field=Field Layer
##Extent=extent
##Grid_method=enum literal rect;hull ;
##Expand_vector=boolean True
##Expand_longitude=number 0.01
##Expand_latitude=number 0.01
##Model=enum multiple Spherical;Exponential;Gaussian;Stein's parameterization
##Estimate_Range_and_Psill=boolean True
##Nugget=number 0
##Range=number 0
##Psill=number 0
##Local_kriging=boolean False
##QgsProcessingParameterNumber|nearest_observations|Number of nearest observations|QgsProcessingParameterNumber.Integer|25
##Show_Sum_of_Square_Errors=boolean False
##Log10_field_kriging=boolean False
##QgsProcessingParameterNumber|Resolution|Resolution (meter)|QgsProcessingParameterNumber.Integer|0
##Color_plots=enum literal turbo;magma;inferno;plasma;viridis;cividis;rocket;mako ;
##Insert_points=boolean True
##Report=output file docx
##Create_report=boolean False
##Kriging_variance= output raster
##Kriging_prediction= output raster

# read packages ===========================================================
packages = c("gstat", "ggpmisc", "ggplot2", "sp", "sf", "raster","officer",
             "tictoc", "cowplot", "viridis", "ggrepel")

for (pac in packages) {
  if (!suppressMessages(require(pac, character.only=TRUE, quietly=TRUE))) {
    install.packages(package, repos = getOption("repos"), dependencies=TRUE)
    cat("package installed:", paste(package, collapse = ", "),"\n")
  }
}

# =========================================================================
tic()

# =========================================================================
# extract crs
crs_info = sf::st_crs(Layer)
# scientific notation
options(scipen = 10000)
# automatic resolution
if(Resolution <= 0){
  Resolution = round(sqrt((Extent[2] - Extent[1])^2 + (Extent[4] - Extent[3])^2)/400)
}

# =========================================================================
plot_variogram = function(vg, fit.vgm, model = NULL) {
  preds = variogramLine(fit.vgm, maxdist = max(vg$dist))
  breaks = pretty(range(vg$np), n = nclass.Sturges(vg$np)-1, min.n = 1)

  mdf = as.data.frame(fit.vgm)[c("psill", "range")]

  if(length(fit.vgm$model) < 2){
    nugget = list(NULL)
  } else {
    nugget = list(geom_segment(data=mdf[1,], aes(x = range, y = 0, xend = range, yend = min(psill), color="Nugget"), linetype = 2))
    mdf = mdf[2,]
  }

  g1 = ggplot(data = vg, aes(x = dist, y = gamma)) +
    theme_bw() +
    geom_line(linetype=2) +
    geom_segment(data=mdf, aes(x = range, y = 0, xend = range, yend = psill, color="Range"), linetype = 2) +
    geom_segment(data=mdf, aes(x = 0, y = psill, xend = Inf, yend = psill, color="Sill"), linetype = 2) +
    nugget +
    geom_point(aes(size=np, fill=np), shape= 21) +
    geom_line(data = preds, aes(x = dist, y = gamma), linewidth = 1.0, inherit.aes = FALSE) +
    geom_text_repel(aes(label = np)) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_size_binned(range = c(1, 10), breaks=breaks) +
    scale_fill_viridis(option = Color_plots) +
    guides(fill = guide_legend(), size = guide_legend()) +
    labs(x = "distance", y = "semivariance", subtitle = "np: the number of point pairs for this estimate",color="")
    return(g1)
}

# =========================================================================
get_grid = function (Layer,
                     Resolution,
                     grid.method = c('rect','hull'),
                     expand = TRUE,
                     fx = 0.02,
                     fy = 0.02
){
  if(class(Layer)[1]=="sf") Layer = as_Spatial(Layer)
  grid.method = match.arg(grid.method) 

           x1 = Layer@bbox[1]
           x2 = Layer@bbox[3]
           y1 = Layer@bbox[2]
           y2 = Layer@bbox[4]

           if(expand)
           {
             x1 = x1 - (x2-x1)*fx
             x2 = x2 + (x2-x1)*fx
             y1 = y1 - (y2-y1)*fy
             y2 = y2 + (y2-y1)*fy
           }

  switch(grid.method,
         rect = {
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
         hull = {
           convex_hull = chull(coordinates(Layer)[, 1], coordinates(Layer)[,2])
           convex_hull = c(convex_hull, convex_hull[1])
           d = Polygon(Layer[convex_hull, ])
         })

  gride = spsample(d, n=1, cellsize=c(Resolution,Resolution), type="regular")
  gridded(gride) = TRUE
  attr(gride, "proj4string") = Layer@proj4string
#   plot(gride)
  return(gride)
}


# =========================================================================
LAYER = as_Spatial(Layer)

LAYER = crop(LAYER, Extent)

names(LAYER)[names(LAYER)==Field] = "Field"
LAYER = remove.duplicates(LAYER)
LAYER = LAYER[!is.na(LAYER$Field),]
LAYER$Field = as.numeric(as.character(LAYER$Field))

if(Log10_field_kriging){
  LAYER$Field = log(LAYER$Field)
  Field = paste0("log(", Field,")")
}

# variogram ===============================================================
g = gstat(id = Field, formula = Field~1, data = LAYER)
vg = variogram(g)

if(Estimate_Range_and_Psill){ Range = NA }
if(Estimate_Range_and_Psill){ Psill = NA }

# model
Model = c("Sph", "Exp", "Gau", "Ste")[Model+1]
vgm_ = vgm(nugget = Nugget, psill = Psill, range = Range, model = Model)
fit_vgm = fit.variogram(vg, vgm_)

if(Show_Sum_of_Square_Errors){ paste("SSE:", attr(fit_vgm, "SSErr")) }

mdf = as.data.frame(fit_vgm)[c("model", "psill", "range")]
if(Create_report)
{
  pv1 = plot_variogram(vg, fit_vgm, Model)
  pv2 = ggplot() +
    theme_void() +
    annotate(geom='table', x=1, y=1, label=list(mdf), size=4)
  p1 = cowplot::plot_grid(pv1, pv2, nrow = 2, ncol = 1, rel_heights = c(6, 1) ) +
    theme(plot.background = element_rect(fill = "white", colour = NA))
}

# grid ====================================================================
GRIDE = get_grid(LAYER, Resolution, grid.method = Grid_method, expand = Expand_vector, fx = Expand_longitude, fy = Expand_latitude)

# kriging =================================================================
kpred = predict(g, newdata = GRIDE)

if(Local_kriging){
  OK = krige(Field~1, LAYER, newdata = kpred, fit_vgm, nmax = nearest_observations)
} else {
  OK = krige(Field~1, LAYER, newdata = kpred, fit_vgm)
}

# plot kriging ============================================================
PRED_RASTER = raster(OK)
PRED_RASTER_DF = as.data.frame(PRED_RASTER, xy = TRUE)
LAYER_DF = as.data.frame(LAYER)

if(Create_report){
  p2 = ggplot() +
    theme_bw() +
    geom_raster(data = PRED_RASTER_DF , aes(x = x, y = y, fill = var1.pred)) +
    ifelse(Insert_points, list(geom_point(data=LAYER_DF, aes(x = x, y = y))), list(NULL)) +
    scale_fill_viridis(option = Color_plots, name=Field) +
    coord_fixed(expand = FALSE) +
    theme(axis.text.y = element_text(angle=90, hjust=.5)) +
    labs(x="longitude", y="latitude",
         caption = paste0("EPSG: ", crs_info$epsg, "; CRS: ",crs_info$input))
}

# plot variogram ==========================================================
VAR_RASTER = raster(OK["var1.var"])
VAR_RASTER_DF = as.data.frame(VAR_RASTER, xy = TRUE)

if(Create_report){
  p3 = ggplot() +
    theme_bw() +
    geom_raster(data = VAR_RASTER_DF, aes(x = x, y = y, fill = var1.var)) +
    ifelse(Insert_points, list(geom_point(data=LAYER_DF, aes(x = x, y = y))), list(NULL)) +
    scale_fill_viridis(option = Color_plots, name=Field) +
    coord_fixed(expand = FALSE) +
    theme(axis.text.y = element_text(angle=90, hjust=.5)) +
    labs(x="longitude", y="latitude",
         caption = paste0("EPSG: ", crs_info$epsg, "; CRS: ",crs_info$input))
}


# out =====================================================================
Kriging_variance = VAR_RASTER
Kriging_prediction = PRED_RASTER

# cross validation ========================================================
KCV = krige.cv(Field~1, LAYER, fit_vgm, nmax = 25, nfold = 5, verbose = FALSE)
KCV_DF = as.data.frame(KCV)
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

# =========================================================================
print.info <- function (...){
  cat("Model:","\n")
  print(mdf, row.names = T)
  cat("\nResolution:", Resolution,"pxs\n")
  cat("\nStatistical:\n")
  cat("Mean error residual, ideally 0:", mean_error_res, "\n")
  cat("MSPE, ideally small:", mspe, "\n")
  cat("Mean square normalized error, ideally: close to 1:", mean_z2, "\n")
  cat("Correlation observed and predicted, ideally 1:", cor_obs_pred, "\n")
  cat("Correlation predicted and residual, ideally 0:", cor_pred_red, "\n")

}
print.info()

# =========================================================================
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
  geom_vline(xintercept = mean_error_res, color="red", linetype=2) +
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
#   scale_fill_gradient2() +
  scale_fill_viridis(option = Color_plots) +
  guides(fill = guide_legend(), size = guide_legend()) +
  theme(plot.title = element_text(size = 12),
#         legend.position = "bottom",
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


title_ = fpar(ftext("Spatial Interpolation - Ordinary Kriging", prop = shortcuts$fp_bold(font.size = 15)))

txt_open = fpar(run_linebreak(),capture.output(print.info()))

txt_crossvalidation = fpar("", run_linebreak())

txt_variogram = fpar("", run_linebreak())

txt_kriging = fpar("",run_linebreak())

txt_variance = fpar("",run_linebreak())

run_num = run_autonum(seq_id = "fig", pre_label = "Figure ", bkm = "figure")

doc = read_docx() |>
  body_add(title_) |>
  body_add(value = txt_open, style = "Normal") |>

  body_add_par(value = "Cross Validation", style = "heading 1") |>
  body_add(value = txt_crossvalidation, style = "Normal") |>
  body_add_gg(value = g1, width = 6.3, height = 6.4, scale= .8, res=150) |>
  body_add_caption(value = block_caption("A figure caption.", style = "Normal", autonum = run_num)) |>

  body_add_par(value = "Variogram", style = "heading 1") |>
  body_add(value = txt_variogram, style = "Normal") |>
  body_add_gg(value = p1, width = 6.3, height = 6.5, res=150) |>
  body_add_caption(value = block_caption("A figure caption.", style = "Normal", autonum = run_num)) |>

  body_add_par(value = "Ordinary krigind simulation", style = "heading 1") |>
  body_add(value = txt_kriging, style = "Normal") |>
  body_add_gg(value = p2, width = 6.3, height = 6.3, res=150) |>
  body_add_caption(value = block_caption(paste("Resolution:", Resolution), style = "Normal", autonum = run_num)) |>

  body_add_par(value = "Ordinary krigind variance", style = "heading 1") |>
  body_add(value = txt_variance, style = "Normal") |>
  body_add_gg(value = p3, width = 6.3, height = 6.3, res=150) |>
  body_add_caption(value = block_caption("A figure caption.", style = "Normal", autonum = run_num)) |>

  body_add_par(value = "Session Info", style = "heading 1") |>
  body_add(value = capture.output(sessionInfo()), style = "Normal")

# arq = file.path(Plots_directory, paste0("report_", periodo, ".docx"))
print(doc, target = Report)
browseURL(Report)
}

nome_doc = paste("kriging on the field vector:", Field)
msg_toc = function(tic,toc,msg,arq){
  paste(" ----------------------------------\n",
        arq, "\n",
        "R algorithm completed in",round(toc - tic, 3), "seconds\n",
        "----------------------------------\n")
}
# =========================================================================
toc(func.toc = msg_toc, arq=nome_doc)
