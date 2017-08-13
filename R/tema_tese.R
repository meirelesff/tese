#' Tema da tese
#'
#' Tema para ggplot2 usado nos graficos da tese
#'
#' @param tam.fonte Font size
#' @param fonte Font type
#'
#'
#' @import ggplot2
#' @export

tema_tese <- function (tam.fonte = 14, fonte = "sans") {

  (ggplot2::theme_bw(base_size = tam.fonte, base_family = fonte) +
     ggplot2::theme(axis.text = ggplot2::element_text(colour = "black", size = ggplot2::rel(1)),
                    axis.title = ggplot2::element_text(colour = "black"),
                    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(0, 15)),
                    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(15)),
                    axis.line.x = ggplot2::element_line(color = "black"),
                    axis.line.y = ggplot2::element_line(color = "black"),
                    axis.ticks = ggplot2::element_blank(),
                    legend.position = "bottom",
                    legend.key = ggplot2::element_blank(),
                    legend.text = ggplot2::element_text(size = tam.fonte),
                    strip.background = ggplot2::element_blank(),
                    strip.text = ggplot2::element_text(size = tam.fonte),
                    panel.grid = ggplot2::element_blank(),
                    panel.border = ggplot2::element_blank(),
                    panel.background = ggplot2::element_blank(),
                    plot.background = ggplot2::element_blank(),
                    plot.title = ggplot2::element_text(size = ggplot2::rel(1), vjust = 0, margin = ggplot2::margin(0, 0, 20, 0))))
}
