#' Marginal Effects para felm
#'
#' Funcao para retornar efeitos marginal para modelos felm com interacoes entre tratamento e variavel discreta
#'
#' @param model Um modelo estimado com o pacote lfe
#' @param base_name Um character com o nome do coeficiente do tratamento
#' @param rem_name Um character com o nome da parte dos coeficientes para remover
#' @param ci Intervalo de confianca
#'
#' @importFrom purrr map_dbl
#' @importFrom stats qnorm
#'
#' @export


marg_felm <- function(model, base_name, rem_name, ci = 0.95){

  # Extract coefs, cov, and number of coefs
  coefs <- model$coefficients
  ccov <- model$clustervcv
  n <- length(coefs)

  # Calculate marginal effects
  marg_coefs <- coefs[1] + coefs[2:n]

  # Calculate confidence intervals
  sqr_covs <- purrr::map_dbl(2:n, ~ ccov[.x, .x])
  sqr_main_cov <- 2 * purrr::map_dbl(2:n, ~ ccov[1, .x])
  se <- sqrt(ccov[1, 1] + sqr_covs + sqr_main_cov)
  ci <- stats::qnorm((1 - (1 - ci) / 2))
  ci_up <- marg_coefs + ci * se
  ci_low <- marg_coefs - ci * se

  # Inclue baseline coef and cis
  marg_coefs <- c(coefs[1], marg_coefs)
  ci_up <- c(coefs[1] + ci * model$cse[1], ci_up)
  ci_low <- c(coefs[1] - ci * model$cse[1], ci_low)

  # Return
  data.frame(vars = c(base_name, gsub(rem_name, "", rownames(coefs)[2:n])),
             coef = marg_coefs, ci_up = ci_up, ci_low = ci_low, stringsAsFactors = FALSE)
}
