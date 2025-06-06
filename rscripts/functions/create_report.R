#-------------------------------------------
# Author      : izi (izi31416@protonmail.com)
# Project     :
# created     : terça mai 20, 2025 12:00:51 -03
# Updated     :
#-------------------------------------------
create_report = function(tit = "Kriging Interpolation",
                         PRED_RASTER,
                         VAR_RASTER,
                         KCV,
                         create.report = TRUE,
                         open.report = TRUE,
                         plot.mask = FALSE,
                         contour.lines = NULL,
                         mask.conf = list(size = 0.3, color = "gray60"),
                         contour.conf = list(size = 0.5, color = "gray50"),
                         color_ramp = palette_colors("Spectral", n = 100, reverse = FALSE)
){
  if(!create.report) return(NULL)

  # PLOT VARIOGRAM ========================================
  pv1 = plot_variogram(vg, var_model, Model)
  pv2 = ggplot() +
    theme_void() +
    annotate(geom='table', x=1, y=1, label=list(VAR_DF), size=4)
  fig_variogram = cowplot::plot_grid(pv1, pv2, nrow = 2, ncol = 1, rel_heights = c(6, 1) ) +
    theme(plot.background = element_rect(fill = "white", colour = NA))

  # PLOT KRIGING =========================================
  PRED_RASTER_DF = as.data.frame(PRED_RASTER, xy = TRUE)
  # remove NA's
#   PRED_RASTER_DF = PRED_RASTER_DF[!is.na(PRED_RASTER_DF$var1.pred),]
  max_pred = max(PRED_RASTER_DF$var1.pred, na.rm = TRUE)
  min_pred = min(PRED_RASTER_DF$var1.pred, na.rm = TRUE)
  LAYER_DF = as.data.frame(LAYER)

#   pr = str2lang(names(PRED_RASTER_DF)[3])

  mask_layer = if(!is.null(Mask_layer) & plot.mask) {
    list(geom_sf(data = mask_crop, fill = NA,
                 linewidth = mask.conf[[1]],
                 color = mask.conf[[2]] ))
  } else list(NULL)

  contour_layer = if(!is.null(contour.lines)) {
    list(geom_sf(data=contour.lines,
                 linewidth = contour.conf[[1]],  color = contour.conf[[2]]
                 ))
  } else list(NULL)

  point = if(Insert_points) {
    list(geom_point(data=LAYER_DF, aes(x = x, y = y), shape=21, fill="white"))
  } else list(NULL)

  fig_pred = ggplot() +
    theme_bw() +
    geom_raster(data = PRED_RASTER_DF, aes(x = x, y = y, fill = var1.pred), na.rm = TRUE) +
    mask_layer +
    contour_layer +
    point +
    scale_fill_gradientn(colors = color_ramp, na.value = NA, limits = c(min_pred,max_pred)) +
#     scale_fill_palette_c(color_ramp) +
    scale_y_continuous(expand = expansion(mult=0.02)) +
    scale_x_continuous(expand = expansion(mult=0.02)) +
    coord_sf(crs = CRS_Layer, datum = CRS_Layer, expand = TRUE, clip = "off") +
    theme(axis.text.y = element_text(angle=90, hjust=.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(x="longitude", y="latitude", caption = epsg_crs_txt, fill = Field)

  # =========================================================================
  if(FALSE)
  {
    #   library(RColorBrewer)
    PRED_RASTER_DF = as.data.frame(PRED_RASTER, xy = TRUE)
    max_pred = max(PRED_RASTER_DF$var1.pred, na.rm = TRUE)
    min_pred = min(PRED_RASTER_DF$var1.pred, na.rm = TRUE)
    LAYER_DF = as.data.frame(LAYER)
    #   PRED_RASTER_DF = PRED_RASTER_DF[!is.na(PRED_RASTER_DF$var1.pred),]

    N_colors = 10
    digits = 3
    #   myPallette = c(hcl.colors(9, "YlOrRd", rev=F), "white", hcl.colors(9, "Blues", rev=T))

    cut_brks <- base::cut(PRED_RASTER_DF$var1.pred,
                          breaks = seq(min_pred, max_pred, len = N_colors),
                          dig.lab = digits)
    level_brks = levels(cut_brks)
    lower = as.numeric( sub("\\((.+),.*", "\\1", level_brks) )
    upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", level_brks) )
    labels_brks = if(F) paste0("-  [", lower, ", ", upper,"]") else  paste0("-  ", lower)
    brks = c(lower, max(upper))

    ggplot(PRED_RASTER_DF, aes(x = x, y = y)) +
      scale_fill_manual(values = palette_colors("Spectral", n = N_colors),
                        na.value = "transparent",
                        na.translate = FALSE,
                        guide = guide_legend(reverse = TRUE)) +
      geom_raster(aes(fill = base::cut(var1.pred, breaks = brks, labels = labels_brks)), na.rm = TRUE) +
      scale_y_continuous(expand = expansion(mult=0.02)) +
      scale_x_continuous(expand = expansion(mult=0.02)) +
      coord_sf(crs = CRS_Layer, datum = CRS_Layer, expand = TRUE, clip = "off") +
      theme(
            legend.text  = element_text(margin = margin(l = 0)),
            legend.title = element_text(margin = margin(b = 5))
            ) +
      labs(fill = Field)
  }


  # PLOT VARIOGRAM ========================================
  VAR_RASTER_DF = as.data.frame(VAR_RASTER, xy = TRUE)
  max_var = max(VAR_RASTER_DF$var1.var, na.rm = TRUE)
  min_var = min(VAR_RASTER_DF$var1.var, na.rm = TRUE)

#   vr = str2lang(names(VAR_RASTER_DF)[3])

  fig_var = ggplot() +
    theme_bw() +
    geom_raster(data = VAR_RASTER_DF, aes(x = x, y = y, fill = var1.var), na.rm = TRUE) +
    mask_layer +
    ifelse(Insert_points, list(geom_point(data=LAYER_DF, aes(x = x, y = y), shape=21, fill="white")), list(NULL)) +
    scale_fill_gradientn(colors = color_ramp, na.value = NA, limits = c(min_var,max_var)) +
#     scale_fill_palette_c(color_ramp) +
    scale_y_continuous(expand = expansion(mult=0.02)) +
    scale_x_continuous(expand = expansion(mult=0.02)) +
    coord_sf(crs = CRS_Layer, datum = CRS_Layer, expand = TRUE, clip = "off") +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(x="longitude", y="latitude", caption = epsg_crs_txt, fill = Field)

  # CLOUD VARIOGRAM =========================================================
  # variogram calculation, cloud=TRUE is for cloud scatter
  meuse.varioc <- variogram(frm, LAYER, cloud=TRUE)

  # Customizing the cloud plot
  fig_cloud = ggplot(meuse.varioc, aes(x=meuse.varioc$dist, y=meuse.varioc$gamma)) +
    geom_point(color = "blue", fill = "blue", size = 2, alpha = 0.25) +
    scale_x_continuous(expand = expansion(mult=0.03)) +
    scale_y_continuous(expand = expansion(mult=0.01)) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(title = NULL, x = "h(m)", y = bquote(gamma~"(h)"))


  # CROSS VALIDATION ========================================================
  KCV_DF = as.data.frame(KCV)
  ST = get_stats(KCV)
  STAT = ST[['stats']]

  # Calculating the Sturges bins
  breaks = pretty(range(KCV$residual), n = nclass.Sturges(KCV$residual), min.n = 1)

  a1 = ggplot(KCV_DF, aes(x = residual)) +
    theme_bw(12) +
    geom_histogram(color = "black", fill = "azure3", breaks = breaks) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult=c(0,0.1))) +
    scale_x_continuous() +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(size = 12)) +
    geom_vline(xintercept = ST[['mean_error_res']], color="red", linetype = 2) +
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
    scale_fill_gradientn(colors = color_ramp, na.value = NA) +
#     scale_fill_palette_c(color_ramp) +
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
    labs(x="Theoretical Quanrasters", y="Residual Quanrasters", title="Q-Q Plot")

  fig_stats = plot_grid(a1,a2,a3,a4)

  title_ = fpar(ftext(tit, prop = shortcuts$fp_bold(font.size = 15)))
  txt_open = fpar(run_linebreak(), "")
  txt_crossvalidation = fpar("", run_linebreak())
  txt_variogram = fpar("", run_linebreak())
  txt_kriging = fpar("",run_linebreak())
  txt_variance = fpar("",run_linebreak())
  txt_kriging_clip = fpar("",run_linebreak())

  mdf_model = as.character(VAR_DF$model)

  if(length(VAR_DF$model) > 1)
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
  cap_table_exp_var = "Exp var."

  cap_fig_g1 = paste("Leave-One-Out Cross-Validation (LOOCV) for the",
                     model_fit, "model validation.")
  cap_fig_fig_variogram = paste0(model_fit, "model fitted to residual values from the first-order trend surface.")
  cap_fig_fig_pred = paste("Final result of interpolation, the prediction map.")
  cap_fig_fig_var = "Final result of interpolation, the variance open.report error map."
  cap_fig_fig_cloud = paste("Variogram Cloud -", Field)
#   cap_fig_p5 = paste("Clip kriging -", Field)

  run_num = run_autonum(seq_id = "fig", pre_label = "Figure ", bkm = "figure")
  run_num_table = run_autonum(seq_id = "tab", pre_label = "Table ", bkm = "table")

  # =========================================================================
#   cap = capture.output(summary(warnings()))
# NOTE: "Verificar erro PCDATA invalid Char value 27 [9], provocado pelo warning do ggplot2

  # =========================================================================
  docx = read_docx() |>
    body_add(title_) |>
    body_add(value = txt_open, style = "Normal") |>
    body_add(value = capture.output(printInfo()), style = "Normal") |>

    body_add_break() |>

    body_add_par(value = "Stats", style = "heading 1") |>
    body_add_par(value = run_linebreak(), style = "Normal") |>
    body_add_caption(value = block_caption(cap_table_model, style = "Normal", autonum = run_num_table)) |>
    body_add_table(VAR_DF, style = "Table Professional") |>
    body_add_par(value = run_linebreak(), style = "Normal") |>
    body_add_caption(value = block_caption(cap_table_stat, style = "Normal", autonum = run_num_table)) |>
    body_add_table(STAT, style = "Table Professional") |>

    body_add_break() |>

    body_add_par(value = "Cross Validation", style = "heading 1") |>
    body_add(value = txt_crossvalidation, style = "Normal") |>
    body_add_gg(value = fig_stats, width = 6.3, height = 6.4, scale= .8, res=150) |>
    body_add_caption(value = block_caption(cap_fig_g1, style = "Normal", autonum = run_num)) |>

    body_add_break() |>

    body_add_par(value = "Variogram", style = "heading 1") |>
    body_add(value = txt_variogram, style = "Normal") |>
    body_add_gg(value = fig_variogram, width = 6.3, height = 6.5, res=150) |>
    body_add_caption(value = block_caption(cap_fig_fig_variogram, style = "Normal", autonum = run_num)) |>
    body_add_par(value = run_linebreak(), style = "Normal") |>
    body_add_caption(value = block_caption(cap_table_exp_var, style = "Normal", autonum = run_num_table)) |>
    body_add_table(vg, style = "Table Professional") |>
    body_add_par(value = run_linebreak(), style = "Normal") |>
    body_add_gg(value = fig_cloud, width = 6.3, height = 6.4, scale= .8, res=150) |>
    body_add_caption(value = block_caption( cap_fig_fig_cloud, style = "Normal", autonum = run_num)) |>

    body_add_break() |>

    body_add_par(value = "Ordinary Kriging simulation", style = "heading 1") |>
    body_add(value = txt_kriging, style = "Normal") |>
    body_add_gg(value = fig_pred, width = 6.3, height = 6.3, res=150) |>
    body_add_caption(value = block_caption(cap_fig_fig_pred, style = "Normal", autonum = run_num)) |>

    body_add_break() |>

    body_add_par(value = "Ordinary Kriging variance", style = "heading 1") |>
    body_add(value = txt_variance, style = "Normal") |>
    body_add_gg(value = fig_var, width = 6.3, height = 6.3, res=150) |>
    body_add_caption(value = block_caption(cap_fig_fig_var, style = "Normal", autonum = run_num)) |>

    body_add_break() |>

#     body_add_par(value = "Warnings", style = "heading 1") |>
#     body_add(value =  cap, style = "Normal") |>

    body_add_par(value = "Session Info", style = "heading 1") |>
    body_add(value = capture.output(sessionInfo()), style = "Normal") |>
    set_doc_properties(title = tit,
                       subject = "log qgis-r",
                       creator = "i3i QGIS-R",
                       contributor = "i3i QGIS-R",
                       description = "log qgis-r",
                       created = Sys.time())

  print(docx, target = Report)
  if(open.report) browseURL(Report)

  return(invisible(list(fig_variogram = fig_variogram,
                        fig_pred = fig_pred,
                        fig_var = fig_var,
                        fig_cloud = fig_cloud,
                        fig_stats = fig_stats,
                        docx = docx)
  ))
}

