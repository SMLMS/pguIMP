#' @title transposeTibble
#'
#' @description
#' Transposes a tibble
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @param obj
#' The data frame to be transposed.
#' (numeric)
#'
#' @return
#' The transposed data frame.
#' (tibble:tibble)
#'
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble tibble
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
    dplyr::select(c("parameter"), dplyr::everything()) %>%
    return()
}
