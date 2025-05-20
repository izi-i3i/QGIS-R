##-------------------------------------------
## Author      : izi (izi31416@protonmail.com)
## Project     :
## created     : terça mai 20, 2025 12:00:51 -03
## Updated     :
##-------------------------------------------
create_report = function(Create_report = FALSE, Open_report = FALSE)
{
  if(!Create_report) return(NULL)

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
    scale_fill_viridis(option = Color_report) +
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
  if(Open_report) browseURL(Report)
}
