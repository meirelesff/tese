#' BW plot
#'
#' Funcao para plotar sensibilidade do estimador RD
#'
#' @param x scores;
#' @param y Var dependente;
#' @param c Cutpoint;
#' @param p Polinomio (default 1);
#' @param h_bw Vetor com os valores inicial, final e intervalos do BW;
#' @param cluster Cluster robust SE;
#' @param level CI;
#'
#' @import ggplot2
#' @import rdrobust
#' @importFrom stats quantile
#'
#' @export
#'
#' @return A ggplot object;

plot_bw <- function(y, x, c = 0, p = 1, h_bw = NULL, cluster = NULL, level = 95){
  
  # Input tests
  if(is.null(h_bw)){
    qnts <- as.numeric(quantile(abs(x), probs = seq(0, 1, by = 0.1), na.rm = T))
    h_bw <- c(qnts[2], qnts[8], 50)
    h <- seq(qnts[2], qnts[8], length.out = 50)
  }
  else {
    
    h <- seq(h_bw[1], h_bw[2], by = h_bw[3])
  }
  # Calculate the estimates
  coef <- ci_up <- ci_low <- numeric(length(h))
  for (i in 1:length(h)){
    reg <- rdrobust::rdrobust(x = x, y = y, c = c, p = p, h = h[i], level = level, cluster = cluster)
    coef[i] <- reg$coef[1]
    ci_up[i] <- reg$ci[1]
    ci_low[i] <- reg$ci[4]
  }
  
  # Plots
  out <- data.frame(coef = coef, ci_up = ci_up, ci_low = ci_low, h = h)
  
  ggplot2::ggplot(out, ggplot2::aes(x = h, y = coef)) + ggplot2::geom_line(size = 0.82) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_up, ymax = ci_low), alpha = 0.2) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::geom_hline(yintercept = c, linetype = "dashed", size = 0.3)
}
