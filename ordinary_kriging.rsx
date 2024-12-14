##Ordinary Kriging=name
##[R-Geostatistics]=group
##Layer=vector
##Field=Field Layer
##Extent=extent
##Grid_method=enum literal rect;hull ;
##Expand_vector=boolean True
##Expand_longitude=number 0.01
##Expand_latitude=number 0.01
##Model=enum multiple Spherical;Exponential;Gaussian;Stein's parameterization ;
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
packages = c("gstat", "ggpmisc", "raster", "ggplot2", "sp", "sf",
             "raster","officer", "tictoc", "cowplot", "viridis", "ggrepel")

for (pac in packages) {
  if (!suppressMessages(require(pac, character.only=TRUE, quietly=TRUE))) {
    install.packages(package, repos = getOption("repos"), dependencies=TRUE)
    cat("package installed:", paste(package, collapse = ", "),"\n")
  }
}

# =========================================================================
tic()

# =========================================================================
# included in the report name
periodo = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
# extract crs
crs_info = sf::st_crs(Layer)
# scientific notation
options(scipen = 10000)
# automatic resolution
if(Resolution <= 0){
  Resolution = round(sqrt((Extent[2] - Extent[1])^2 + (Extent[4] - Extent[3])^2)/500)
}


# =========================================================================
scientific_10 = function(x) {
  ifelse(x==0, "0", parse(text = sub("e[+]?", " %*% 10^", scales::scientific_format()(x)))
  )
}

# =========================================================================
plot_variogram = function(vg, fit.vgm, model = NULL) {
  preds = variogramLine(fit.vgm, maxdist = max(vg$dist))
  breaks = pretty(range(vg$np), n = nclass.Sturges(vg$np)-1, min.n = 1)

  g1 = ggplot(data = vg, aes(x = dist, y = gamma)) +
    theme_bw() +
    geom_point(aes(color=np, size=np), shape = 19, alpha = 1.0, show.legend = T) +
    geom_text_repel(aes(label = np)) +
    scale_size_binned(range = c(1, 10), breaks=breaks) +
    scale_color_viridis(option = Color_plots) +
    geom_line(data = preds, aes(x = dist, y = gamma), inherit.aes = F) +
    coord_cartesian(clip = 'off', ylim = c(0,NA), xlim=c(0,NA)) +
    guides(color = guide_legend(), fill = guide_legend(), size = guide_legend()) +
    labs(x = "distance", y = "semivariance")
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
vgm = vgm(nugget = Nugget, psill = Psill, range = Range, model = Model)
fit_vgm = fit.variogram(vg, vgm)

if(Show_Sum_of_Square_Errors){ paste("SSE:", attr(fit_vgm, "SSErr")) }

if(Create_report)
{
  pv1 = plot_variogram(vg, fit_vgm, Model)
  pv2 = ggplot() +
    theme_void() +
    annotate(geom='table', x=1, y=1, label=list(fit_vgm), size=4)
  p1 = cowplot::plot_grid(pv1, pv2, nrow = 2, ncol = 1, rel_heights = c(5, 1) )
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

# =========================================================================
if(Create_report)
{
# cross validation ========================================================
KCV = krige.cv(Field~1, LAYER, fit_vgm, nmax = 25, nfold = 5, verbose = FALSE)
KCV_DF = as.data.frame(KCV)
# Mean error, ideally 0:
mean_error_res = mean(KCV$residual)
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

# =========================================================================
# MSPE, ideally small
mspe = mean(KCV$residual^2)
# Mean square normalized error, ideally close to 1
mean_z2 = mean(KCV$zscore^2)
# Correlation observed and predicted, ideally 1
cor_obs_pred = cor(KCV$observed, KCV$var1.pred)
# Correlation predicted and residual, ideally 0
cor_pred_red = cor(KCV$var1.pred, KCV$residual)

Result = c(mean_error_res, mspe, mean_z2, cor_obs_pred, cor_pred_red)
Statistic = c("Mean error", "MSPE", "Mean square normalized error", "Correlation observed and predicted", "Correlation predicted and residual")
Idealy = c("0","small","close to 1","1","0")

Stat_DF = data.frame(Statistic, Result, Idealy)


# ggplot(KCV_DF, aes(sample = rstandard(lm(var1.pred ~ observed)))) +
#   stat_qq(size=2) +
#   stat_qq_line(line.p = c(.25, .75)) +
#   labs(title="Normal Q-Q", x="Theoretical Quantiles", y="Standardized Residuals")

title_ = fpar(ftext("Spatial interpolation", prop = shortcuts$fp_bold(font.size = 15)))

txt_crossvalidation = fpar(run_linebreak(),paste("Resolution:", Resolution), run_linebreak())

txt_variogram = fpar(run_linebreak(),"Spatial interpolation techniques are used to estimate the values of variables at unsampled locations based on the values of the same variable at sampled locations. One of the popular spatial interpolation techniques used in geostatistics is Kriging interpolation. Kriging interpolation is a powerful statistical method that allows one to predict the values of variables at unsampled locations while also accounting for spatial autocorrelation.", run_linebreak())

txt_kriging = fpar(run_linebreak(),"Spatial interpolation techniques are used to estimate the values of variables at unsampled locations based on the values of the same variable at sampled locations. One of the popular spatial interpolation techniques used in geostatistics is Kriging interpolation. Kriging interpolation is a powerful statistical method that allows one to predict the values of variables at unsampled locations while also accounting for spatial autocorrelation.",run_linebreak())

txt_variance = fpar(run_linebreak(),"Spatial interpolation techniques are used to estimate the values of variables at unsampled locations based on the values of the same variable at sampled locations. One of the popular spatial interpolation techniques used in geostatistics is Kriging interpolation. Kriging interpolation is a powerful statistical method that allows one to predict the values of variables at unsampled locations while also accounting for spatial autocorrelation.",run_linebreak())

run_num = run_autonum(seq_id = "fig", pre_label = "Figure ", bkm = "figure")

doc = read_docx() |>
  body_add(title_) |>

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
  body_add_caption(value = block_caption("A figure caption.", style = "Normal", autonum = run_num)) |>

  body_add_par(value = "Ordinary krigind variance", style = "heading 1") |>
  body_add(value = txt_variance, style = "Normal") |>
  body_add_gg(value = p3, width = 6.3, height = 6.3, res=150) |>
  body_add_caption(value = block_caption("A figure caption.", style = "Normal", autonum = run_num)) |>

  body_add_par(value = "Session Info", style = "heading 1") |>
  body_add(value = capture.output(sessionInfo()), style = "Normal")

print(doc, target = Report)
browseURL(Report)
}

nome_doc = paste("kriging on the vector", Field)
msg_toc = function(tic,toc,msg,arq){
  paste(" ----------------------------------\n",
        arq, "\n",
        "R algorithm completed in",round(toc - tic, 3), "seconds\n",
        "----------------------------------\n")
}
toc(func.toc = msg_toc, arq=nome_doc)

