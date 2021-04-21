#' Imports a dataset to the shiny pguIMP web interface.
#'
#' Extracts import options from a pguIMP::file instance
#' and imports the desired record based on the passed information.
#' @import R6
#' @import tidyverse
#' @param obj
#' Instance of type R6 and pguIMP::pgu.file
#' @export
#' @return
#' A data frame
#' (tibble::tibble)
#'

importDataSet = function(obj = "pgu.file")
{
  if(!R6::is.R6(obj))
  {
    errorString <- sprintf("\nError:\npguIMP::importDataSet needs obj of type R6!")
    stop(errorString)
  }
  if(class(obj)[1] != "pgu.file")
  {
    errorString <- sprintf("\nError:\nphuIMP::importDataSet needs obj of class pguIMP::pgi.file!")
  }


  switch(obj$suffix,
         csv = {
           return(readr::read_csv(file = obj$uploadFileName,
                                  col_names = obj$columnNames,
                                  col_types = readr::cols(),
                                  skip = obj$skipRows,
                                  na = obj$naChar))
         },
         txt = {
           return(readr::read_delim(file = obj$uploadFileName,
                                    delim = obj$separator,
                                    col_names = obj$columnNames,
                                    col_types = readr::cols(),
                                    skip = obj$skipRows,
                                    na = obj$naChar))
         },
         xls = {
           return(readxl::read_xls(path = obj$uploadFileName,
                                         sheet = obj$sheetIndex,
                                         col_names = obj$columnNames,
                                         skip = obj$skipRows,
                                         na = obj$naChar))
           },
         xlsx = {return(readxl::read_xlsx(path = obj$uploadFileName,
                                          sheet = obj$sheetIndex,
                                          col_names = obj$columnNames,
                                          skip = obj$skipRows,
                                          na = obj$naChar))
         }
  )
}#end importDataSet
