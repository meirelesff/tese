#' CL boot mean
#'
#' Funcao para calcular CI de medias com cluster bootstrap
#'
#' @param times Numero de drawns
#' @param x variavel de interesse
#' @param id cluster
#' @param pval CI
#'
#' @export


cl_boot_mean <- function(times, x, id, pval){

  # Function to sample groups
  sample_groups <- function(x, id){

    df <- data.frame(x = x, id = id)
    grps <- unique(id)
    tam <- length(grps)

    df2 <- data.frame(id = sample(grps, tam, replace = T))
    df2 <-  dplyr::left_join(df2, df, by = "id")
    as.numeric(df2$x)
  }

  # Boot
  m <- replicate(times, mean(sample_groups(x, id), na.rm = T))
  data.frame(mean = mean(x, na.rm = T),
             dw = stats::quantile(m, pval / 2, na.rm = T),
             up = stats::quantile(m, 1 - (pval / 2), na.rm = T)
  )
}
