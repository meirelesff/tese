#' DiD coef
#'
#' Funcao para retornar estimativas did com intervalos de confianca robustos
#'
#' @param modelo Um modelo estimado com o pacote lfe
#' @param grupos Um character com o nome dos coeficientes
#' @param vars Um vetor numerico com a posicao dos coeficiente
#'
#' @export

did_coef <- function(modelo, grupos, vars = 1){

  data.frame(coef = modelo$coefficients[vars],
             se = modelo$cse[vars],
             ci_up = modelo$coefficients[vars] + (1.96 * modelo$cse[vars]),
             ci_low = modelo$coefficients[vars] - (1.96 * modelo$cse[vars]),
             grupos = grupos)
}
