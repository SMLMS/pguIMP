#' @title pgu.missingsCharacterizer
#'
#' @description
#' A class that characterizes the origin of missing values.
#'
#' @details
#' A class that characterizes the origin of missing values.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom dplyr add_row all_of filter pull select select_if
#' @importFrom finalfit missing_compare missing_pairs
#' @importFrom ggplot2 element_rect element_text theme theme_linedraw
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom shiny Progress
#' @importFrom stringr str_subset
#' @importFrom tibble is_tibble tibble
#' @importFrom rlang sym
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'



pgu.missingsCharacterizer <- R6::R6Class("pgu.missingsCharacterizer",
                                         ####################
                                         # instance variables
                                         ####################
                                         private = list(
                                           .featureAlphabet = "character",
                                           .featureAgent = "factor",
                                           .missingsCharacteristics_df = "tbl_df"
                                         ), #private
                                         ##################
                                         # accessor methods
                                         ##################
                                         active = list(
                                           #' @field featureAlphabet
                                           #' Returns the instance variable featureAlphabet.
                                           #' (character)
                                           featureAlphabet = function(){
                                             return(private$.featureAlphabet)
                                           },
                                           #' @field featureAgent
                                           #' Returns the instance variable featureAgent.
                                           #' (character)
                                           featureAgent = function(){
                                             return(as.character(private$.featureAgent))
                                           },
                                           #' @field setFeatureAgent
                                           #' Sets the instance variable featureAgent.
                                           #' (character)
                                           setFeatureAgent = function(agent = "character") {
                                             private$.featureAgent <- factor(agent, levels = self$featureAlphabet)
                                           },
                                           #' @field missingsCharacteristics_df
                                           #' Returns the instance variable missingsCharacteristics_df.
                                           #' (tibble::tibble)
                                           missingsCharacteristics_df = function(){
                                             return(private$.missingsCharacteristics_df)
                                           }
                                         ), #active
                                         ###################
                                         # memory management
                                         ###################
                                         public = list(
                                           #' @description
                                           #' Creates and returns a new `pgu.missingsCharacterizer` object.
                                           #' @param data_df
                                           #' The data to be analyzed.
                                           #' (tibble::tibble)
                                           #' @return
                                           #' A new `pgu.missingsCharacterizer` object.
                                           #' (pguIMP::pgu.missingsCharacterizer)
                                           initialize = function(data_df = "tbl_df"){
                                             self$reset(data_df)
                                           }, #function

                                           #' @description
                                           #' Clears the heap and
                                           #' indicates if instance of `pgu.missingsCharacterizer` is removed from heap.
                                           finalize = function(){
                                             print("Instance of pgu.missingsCharacterizer removed from heap")
                                           },
                                           #' @description
                                           #' Prints instance variables of a `pgu.missingsCharacterizer` object.
                                           #' @return
                                           #' string
                                           print = function(){
                                             rString <- sprintf("\npgu.missingsCharacterizer\n")
                                             cat(rString)
                                             cat("\nmissings Characteristics\n")
                                             self$missingsCharacteristics_df %>%
                                               print()
                                             cat("\n\n")
                                             invisible(self)
                                           }, #print

                                           #####################
                                           # analyze functions #
                                           #####################
                                           #' @description
                                           #' Takes a dataframe that will be analyzed using the analyze function
                                           #' and resets the instance variables.
                                           #' @param data_df
                                           #' The data to be analyzed.
                                           #' (tibble::tibble)
                                           reset = function(data_df = "tbl_df"){
                                             if(!tibble::is_tibble(data_df)){
                                               data_df <- tibble::tibble(NULL)
                                             }
                                             private$.featureAlphabet <- colnames(data_df)
                                             private$.featureAgent <- colnames(data_df)[1]
                                             private$.missingsCharacteristics_df <- tibble::tibble(dependent = character(0),
                                                                                                   explanatory = character(0),
                                                                                                   existings = character(0),
                                                                                                   missings = character(0),
                                                                                                   pValue = character(0))
                                           },
                                           #' @description
                                           #' resets the instance variables and analyzes a dataframe.
                                           #' @param data_df
                                           #' The data to be analyzed.
                                           #' (tibble::tibble)
                                           #' @param progress
                                           #' If shiny is loaded, the analysis' progress is stored within this instance of the shiny Progress class.
                                           #' (shiny::Progress)
                                           analyze  = function(data_df = "tbl_df", progress = "Progress"){
                                             if(!tibble::is_tibble(data_df)){
                                               data_df <- tibble::tibble(NULL)
                                             }
                                             data_df %>%
                                               dplyr::select_if(is.numeric) %>%
                                               self$reset()
                                             for (feature in self$featureAlphabet){
                                               if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                                 progress$inc(1)
                                               }#if
                                               self$setFeatureAgent <- feature
                                               contains_na <- data_df %>%
                                                 dplyr::pull(self$featureAgent) %>%
                                                 anyNA()

                                               if(!contains_na){
                                                 next
                                               } #if

                                               explanatory_vec <- self$featureAlphabet %>%
                                                 stringr::str_subset(pattern = self$featureAgent, negate = TRUE)
                                               for(explanatory in explanatory_vec){
                                                 subset_df <- data_df %>%
                                                   dplyr::select(dplyr::all_of(c(self$featureAgent, explanatory))) %>%
                                                   dplyr::filter(!is.na(!!rlang::sym(explanatory)))

                                                 subset_contains_na <- subset_df %>%
                                                   dplyr::pull(self$featureAgent) %>%
                                                   anyNA()

                                                 if(!subset_contains_na){
                                                   next
                                                 } #if

                                                 subset_missings_df <- subset_df %>%
                                                   as.data.frame() %>%
                                                   finalfit::missing_compare(dependent = self$featureAgent,
                                                                             explanatory = explanatory,
                                                                             digits = c(3,3,10))

                                                 subset_missing  <- subset_missings_df %>%
                                                   dplyr::pull(!!rlang::sym("Missing"))

                                                 subset_existing <- subset_missings_df %>%
                                                   dplyr::pull(!!rlang::sym("Not missing"))

                                                 subset_pValue <- subset_missings_df %>%
                                                   dplyr::pull(!!rlang::sym("p"))

                                                 private$.missingsCharacteristics_df <- self$missingsCharacteristics_df %>%
                                                   dplyr::add_row(dependent = self$featureAgent,
                                                                  explanatory = explanatory,
                                                                  missings= subset_missing,
                                                                  existings = subset_existing,
                                                                  pValue = subset_pValue)
                                               } #for
                                             }#for
                                           }, #function

                                           ##################
                                           # plot functions #
                                           ##################

                                           #' @description
                                           #' Plots the analysis result.
                                           #' @param data_df
                                           #' The data to be analyzed.
                                           #' (tibble::tibble)
                                           plot_pair_dist = function(data_df = "tbl_df"){
                                             if(length(self$featureAlphabet)>10){
                                               return(NULL)
                                             }
                                             else{
                                               p <- data_df %>%
                                                 dplyr::select(dplyr::all_of(self$featureAlphabet)) %>%
                                                 finalfit::missing_pairs(showYAxisPlotLabels = TRUE) +
                                                 # ggplot2::theme_linedraw() +
                                                 ggplot2::theme(
                                                   panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                                   plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                                   legend.background = ggplot2::element_rect(fill = "transparent"),
                                                   legend.key = ggplot2::element_rect(fill = "transparent"),
                                                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
                                                 )
                                               return(p)
                                             }
                                           }#function
                                         ) #public
) #class
