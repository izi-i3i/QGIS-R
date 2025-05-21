##-------------------------------------------
## Author  : Izi (izi31416@protonmail.com)
## Project :
## Created : segunda nov 16, 2024 14:09:15 -03
## License :
## Updated :
##-------------------------------------------
plot_variogram = function(vg, fit.vgm, model = NULL) {
  preds = variogramLine(fit.vgm, maxdist = max(vg$dist))
  breaks = pretty(range(vg$np), n = nclass.Sturges(vg$np)-1, min.n = 1)

  VAR_DF = as.data.frame(fit.vgm)[c("psill", "range")]

  if(length(fit.vgm$model) < 2)
  {
    nrp_line = list(
      geom_hline(yintercept = max(VAR_DF$psill), linetype = 2, color = "gray55"),
      geom_vline(xintercept = max(VAR_DF$range), linetype = 2, color = "gray55"),
       geom_segment(data=VAR_DF, aes(x = 0, y = 0, xend = range, yend = 0, color="Range"),
                    linetype = 1, arrow = arrow(length = unit(0.15,"cm"), ends="both")),
       geom_segment(data=VAR_DF, aes(x = max(range), y=0, xend=max(range), yend=max(psill), color="Psill"),
                    linetype = 1, arrow = arrow(length = unit(0.15,"cm"), ends="both"))
    )
  } else {
    L1 = data.frame(x = 0, y = 0, xend = 0, yend = min(VAR_DF$psill))
    L2 = data.frame(x = 0, y = min(VAR_DF$psill), xend = max(VAR_DF$range), yend = min(VAR_DF$psill))
    L3 = data.frame(x = max(VAR_DF$range), y = min(VAR_DF$psill), xend = max(VAR_DF$range), yend = max(VAR_DF$psill))

    nrp_line = list(
      geom_hline(yintercept=max(VAR_DF$psill), linetype=2, color="gray55"),
      geom_vline(xintercept=max(VAR_DF$range), linetype=2, color="gray55"),
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
    scale_fill_viridis(option = Color_report) +
    guides(fill = guide_legend(order=0), size = guide_legend(order=0), color = guide_legend()) +
    labs(x = "distance", y = "semivariance", fill = "n point pairs", color = "")

    return(g1)
}
