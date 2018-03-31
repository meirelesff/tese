#' Point estimate RD
#'
#' Plota varios estimadores RD lado a lado
#'
#'
#' @param modelos Lista com modelos estimados com rdrobust
#' @param vars Vetor com o nome das variaveis
#'
#' @import ggplot2
#' @export

point_rd <- function(modelos, vars){


  # Prepara as estimativas
  coef <- ci_up <- ci_low <- numeric(length(modelos))

  for(i in 1:length(modelos)){

    coef[i] <- modelos[[i]]$coef[1]
    ci_up[i] <- modelos[[i]]$ci[6]
    ci_low[i] <- modelos[[i]]$ci[3]
  }

  # Cria um df
  df <- data.frame(coef = coef, ci_up = ci_up, ci_low = ci_low, vars = vars)


  # Plota
  ggplot2::ggplot(df, ggplot2::aes(x = vars, y = coef, ymax = ci_up, ymin = ci_low)) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 0.3)
}
