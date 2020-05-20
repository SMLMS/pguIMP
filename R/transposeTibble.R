#' @title transposeTibble
#'
#' @description
#' Transposes a tibble
#'
#' @param obj
#' The data frame to be transposed.
#' (numeric)
#'
#' @return
#' The transposed data frame.
#' (tibble:tibble)
#'
#' @examples
#' y <- tansposeTibble(obj=df)
#'
#' @import tidyverse
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

transposeTibble = function(obj = "tbl_df"){
  vNames <- colnames(obj)[-1]
  cNames <- obj %>%
    dplyr::pull(1)
  obj[-1] %>%
    as.data.frame() %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(~ c(cNames)) %>%
    dplyr::mutate(parameter = vNames) %>%
    dplyr::select(parameter, dplyr::everything()) %>%
    return()
}
