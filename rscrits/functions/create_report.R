##-------------------------------------------
## Author      : izi (izi31416@protonmail.com)
## Project     :
## created     : terça mai 20, 2025 12:00:51 -03
## Updated     :
##-------------------------------------------
create_report = function(cr = TRUE, or = FALSE)
{
  if(!cr) return(NULL)


  # PLOT VARIOGRAM ========================================
  pv1 = plot_variogram(vg, var_model, Model)
  pv2 = ggplot() +
    theme_void() +
    annotate(geom='table', x=1, y=1, label=list(VAR_DF), size=4)
  p1 = cowplot::plot_grid(pv1, pv2, nrow = 2, ncol = 1, rel_heights = c(6, 1) ) +
    theme(plot.background = element_rect(fill = "white", colour = NA))


  # PLOT KRIGING =========================================
  PRED_RASTER_DF = as.data.frame(PRED_RASTER, xy = TRUE)
  LAYER_DF = as.data.frame(LAYER)

  p2 = ggplot() +
    theme_bw() +
    geom_raster(data = PRED_RASTER_DF , aes(x = x, y = y, fill = var1.pred)) +
    ifelse(Insert_points, list(geom_point(data=LAYER_DF, aes(x = x, y = y), shape=20)), list(NULL)) +
#     scale_fill_viridis(option = Color_report, name=Field, na.value="transparent") +
    scale_fill_palette_c(Color_report) +
    scale_y_continuous(expand = expansion(mult=0.01)) +
    scale_x_continuous(expand = expansion(mult=0.01)) +
    coord_fixed(expand = TRUE, clip = "off") +
    theme(axis.text.y = element_text(angle=90, hjust=.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(x="longitude", y="latitude", caption = epsg_crs_txt)

  # PLOT VARIOGRAM ========================================
  VAR_RASTER_DF = as.data.frame(VAR_RASTER, xy = TRUE)

  p3 = ggplot() +
    theme_bw() +
    geom_raster(data = VAR_RASTER_DF, aes(x = x, y = y, fill = var1.var)) +
    ifelse(Insert_points, list(geom_point(data=LAYER_DF, aes(x = x, y = y), shape=20)), list(NULL)) +
#     scale_fill_viridis(option = Color_report, name=Field, na.value="transparent") +
    scale_fill_palette_c(Color_report) +
    scale_y_continuous(expand = expansion(mult=0.01)) +
    scale_x_continuous(expand = expansion(mult=0.01)) +
    coord_fixed(expand = TRUE, clip = "off") +
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(x="longitude", y="latitude", caption = epsg_crs_txt)

  # CLOUD VARIOGRAM =========================================================
  # variogram calculation, cloud=TRUE is for cloud scatter 
  meuse.varioc <- variogram(frm, LAYER, cloud=TRUE)

  # Customizing the cloud plot
  p4 = ggplot(meuse.varioc, aes(x=meuse.varioc$dist, y=meuse.varioc$gamma)) + 
    geom_point(color = "blue", fill = "blue", size = 2, alpha = 0.25) +
    scale_x_continuous(expand = expansion(mult=0.03)) +
    scale_y_continuous(expand = expansion(mult=0.01)) +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white')
          ) +
    labs(title = NULL,
         x = "h(m)",
         y = bquote(gamma~"(h)"))

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
#     scale_fill_viridis(option = Color_report) +
    scale_fill_palette_c(Color_report) +
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
  cap_fig_p1 = paste0(model_fit, "model fitted to residual values from the first-order trend surface.")
  cap_fig_p2 = paste("Final result of interpolation, the prediction map.")
  cap_fig_p3 = "Final result of interpolation, the variance or error map."
  cap_fig_p4 = paste("Variogram Cloud -", Field)

  run_num = run_autonum(seq_id = "fig", pre_label = "Figure ", bkm = "figure")
  run_num_table = run_autonum(seq_id = "tab", pre_label = "Table ", bkm = "table")

  doc = read_docx() |>
    body_add(title_) |>
    body_add(value = txt_open, style = "Normal") |>
    body_add(value = capture.output(printInfo()), style = "Normal") |>
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
    body_add_gg(value = g1, width = 6.3, height = 6.4, scale= .8, res=150) |>
    body_add_caption(value = block_caption(cap_fig_g1, style = "Normal", autonum = run_num)) |>

    body_add_break() |>

    body_add_par(value = "Variogram", style = "heading 1") |>
    body_add(value = txt_variogram, style = "Normal") |>
    body_add_gg(value = p1, width = 6.3, height = 6.5, res=150) |>
    body_add_caption(value = block_caption(cap_fig_p1, style = "Normal", autonum = run_num)) |>
    body_add_par(value = run_linebreak(), style = "Normal") |>
    body_add_caption(value = block_caption(cap_table_exp_var, style = "Normal", autonum = run_num_table)) |>
    body_add_table(exp_var, style = "Table Professional") |>
    body_add_par(value = run_linebreak(), style = "Normal") |>
    body_add_gg(value = p4, width = 6.3, height = 6.4, scale= .8, res=150) |>
    body_add_caption(value = block_caption( cap_fig_p4, style = "Normal", autonum = run_num)) |>

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

    body_add_par(value = "Warnings", style = "heading 1") |>
    body_add(value = capture.output(summary(warnings())), style = "Normal") |>

    body_add_par(value = "Session Info", style = "heading 1") |>
    body_add(value = capture.output(sessionInfo()), style = "Normal")

  print(doc, target = Report)
  if(or) browseURL(Report)

  return(invisible(list(p1=p1, p2=p2, p3=p3, p4=p4)))
}

