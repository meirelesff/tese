#' RD interna
#'
#' Funcao para resumir modelos RD (com notacao para LaTeX)
#'
#' @param modelo Um modelo estimado com o pacote rdrobust
#' @param var Um character com o nome do modelo
#' @param vert O modelo pode ser reportado na vertical?
#'
#'
#' @import dplyr
#' @export

rd_internal <- function(modelo, var, vert = T){


  # Posicao vertical
  if(vert){

    # Nomes das linhas
    nomes <- c("Efeito", " ", "P-valor", "H", "N")

    # Estatisticas
    pval <- modelo$pv[3]
    coef <- dplyr::case_when(
      pval < 0.01 ~ paste0(round(modelo$coef[3], 2), "\\textsuperscript{***}"),
      pval < 0.05 ~ paste0(round(modelo$coef[3], 2), "\\textsuperscript{**}"),
      pval < 0.1 ~ paste0(round(modelo$coef[3], 2), "\\textsuperscript{*}"),
      pval >= 0.1 ~ paste0(round(modelo$coef[3], 2))
    )
    se <- paste0("(", round(modelo$se[3], 2), ")")
    h <- round(modelo$bws[1], 2)
    n <- sum(modelo$Nh)

    # Cria o df
    df <- data.frame(x = c(coef, se, round(pval, 2), h, n))
    row.names(df) <- nomes
    colnames(df)[1] <- var

    # Exporta
    return(df)
  }

  # Nomes das linhas
  nomes <- c("Var.", "Efeito", "Erro-padrao", "P-valor", "H", "N")

  # Estatisticas
  pval <- modelo$pv[3]
  coef <- dplyr::case_when(
    pval < 0.01 ~ paste0(round(modelo$coef[3], 2), "\\textsuperscript{***}"),
    pval < 0.05 ~ paste0(round(modelo$coef[3], 2), "\\textsuperscript{**}"),
    pval < 0.1 ~ paste0(round(modelo$coef[3], 2), "\\textsuperscript{*}"),
    pval >= 0.1 ~ paste0(round(modelo$coef[3], 2))
  )
  se <-round(coef$se[3], 2)
  h <- round(modelo$bws[1], 2)
  n <- sum(modelo$Nh)

  # Cria o df
  df <- data.frame(var = var, coef = coef, se = se, pval = round(pval, 2), h = h, n = n)
  names(df) <- nomes

  # Exporta
  return(df)
}



#' RD vertical
#'
#' Funcao para resumir varios modelos RD em colunas verticais
#'
#' @param modelos Uma lista contendo os modelos
#' @param vars Um vetor contendo o nome das variaveis
#'
#' @export

rd_vertical <- function(modelos, vars){

  res <- vector("list", length(modelos))

  for(i in 1:length(modelos)){

    res[[i]] <- rd_internal(modelos[[i]], vars[i])
  }

  Reduce("cbind", res)
}


#' RD horizontal
#'
#' Funcao para resumir varios modelos RD em linhas horizontais
#'
#' @param modelos Uma lista contendo os modelos
#' @param vars Um vetor contendo o nome das variaveis
#'
#' @export

rd_horizontal <- function(modelos, vars){

  res <- vector("list", length(modelos))

  for(i in 1:length(modelos)){

    res[[i]] <- rd_internal(modelos[[i]], vars[i], F)
  }

  Reduce("rbind", res)
}
