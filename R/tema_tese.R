#' Tema da tese
#'
#' Tema para ggplot2 usado nos graficos da tese
#'
#' @param tam.fonte Font size
#' @param fonte Font type
#' @param rel.eixo Axis text size
#'
#'
#' @import ggplot2
#' @export

tema_tese <- function (tam.fonte = 12, fonte = "sans", rel.eixo = 0.8) {

  (ggplot2::theme_bw(base_size = tam.fonte, base_family = fonte) +
     ggplot2::theme(axis.text = ggplot2::element_text(colour = "black", size = ggplot2::rel(rel.eixo), color = "black"),
                    axis.title = ggplot2::element_text(colour = "black"),
                    axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(0, 15), color = "black"),
                    axis.title.x = ggplot2::element_text(margin = ggplot2::margin(15), color = "black"),
                    axis.line.x = ggplot2::element_line(color = "black"),
                    axis.line.y = ggplot2::element_line(color = "black"),
                    legend.position = "bottom",
                    legend.key = ggplot2::element_blank(),
                    legend.text = ggplot2::element_text(size = tam.fonte),
                    strip.background = ggplot2::element_blank(),
                    strip.text = ggplot2::element_text(size = tam.fonte),
                    panel.grid = ggplot2::element_blank(),
                    panel.border = ggplot2::element_blank(),
                    panel.background = ggplot2::element_blank(),
                    plot.background = ggplot2::element_blank(),
                    plot.title = ggplot2::element_text(size = ggplot2::rel(1.05), color = "black", vjust = 0, margin = ggplot2::margin(0, 0, 20, 0))))
}
