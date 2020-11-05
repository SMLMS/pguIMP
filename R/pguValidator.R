#' @title pgu.validator
#'
#' @description
#' Validates that the distribution is not significantly altered by the imputation process.
#'
#' @details
#' Takes two distributions (before and after imputation).
#' Performs a Wilcoxon-Mann-Whitney U test.
#' Performs a Kolmogorow-Smirnow test.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.validator$new()
#'
#' @import R6
#' @import tidyverse
#' @import gridExtra
#' @import stats
#' @import e1071
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.validator <- R6::R6Class("pgu.validator",
                             ####################
                             # instance variables
                             ####################
                             private = list(
                               .testStatistics_df = "tbl_df",
                               .centralMoments_org = "tbl_df",
                               .centralMoments_imp = "tbl_df",
                               .centralMoments_delta = "tbl_df",
                               .features = "character",
                               .seed = "integer"
                             ), #private
                             ##################
                             # accessor methods
                             ##################
                             active = list(
                               #' @field testStatistics_df
                               #' Returns the instance variable `testStatistics_df`.
                               #' (tibble::tibble)
                               testStatistics_df = function(){
                                 return(private$.testStatistics_df)
                               },
                               #' @field centralMoments_org
                               #' Returns the instance variable `centralMoments_org`
                               #' (tibble::tibble)
                               centralMoments_org = function(){
                                 return(private$.centralMoments_org)
                               },
                               #' @field centralMoments_imp
                               #' Returns the instance variable `centralMoments_imp`
                               #' (tibble::tibble)
                               centralMoments_imp = function(){
                                 return(private$.centralMoments_imp)
                               },
                               #' @field centralMoments_delta
                               #' Returns the instance variable `centralMoments_delta`
                               #' (tibble::tibble)
                               centralMoments_delta = function(){
                                 return(private$.centralMoments_delta)
                               },
                               #' @field features
                               #' Returns the instance variable `features`
                               #' (character)
                               features = function(){
                                 return(private$.features)
                               },
                               #' @field seed
                               #' Returns the instance variable seed
                               #' (integer)
                               seed = function(){
                                 return(private$.seed)
                               },
                               #' @field setSeed
                               #' Sets the instance variable seed.
                               #' (numeric)
                               setSeed = function(value = "numeric"){
                                 private$.seed <- value
                               }
                             ),#active
                             ###################
                             # memory management
                             ###################
                             public = list(
                               #' @description
                               #' Creates and returns a new `pgu.validator` object.
                               #' @param seed
                               #' Set the instance variable `seed`.
                               #' (integer)
                               #' @return
                               #' A new `pgu.validator` object.
                               #' (pguIMP::pgu.validator)
                               #' @examples
                               #' x <- pguIMP:pgu.validator$new(seed = 42)
                               initialize = function(seed = 42){
                                 self$setSeed <- seed
                                 self$resetValidator()
                               },
                               #' @description
                               #' Clears the heap and
                               #' indicates that instance of `pgu.validator` is removed from heap.
                               finalize = function(){
                                 print("Instance of pgu.validator removed from heap")
                               },
                               ##########################
                               # print instance variables
                               ##########################
                               #' @description
                               #' Prints instance variables of a `pgu.validator` object.
                               #' @return
                               #' string
                               #' @examples
                               #' x$print()
                               #' print(x)
                               print = function(){
                                 rString <- sprintf("\npgu.validator:\n")
                                 cat(rString)
                                 print(self$features)
                                 print(self$testStatistics_df)
                                 print(self$centralMoments_org)
                                 print(self$centralMoments_imp)
                                 print(self$centralMoments_delta)
                                 cat("\n\n")
                                 invisible(self)
                               }, #function
                               ####################
                               # public functions #
                               ####################
                               #' @description
                               #' Resets instance variables
                               #' @examples
                               #' x$resetValidator()
                               resetValidator = function(){
                                 private$.testStatistics_df <- tibble::tibble(feature = character(0),
                                                                              d.Kolmogorow = numeric(0),
                                                                              p.Kolmogorow = numeric(0),
                                                                              w.Wilcoxon = numeric(0),
                                                                              p.Wilcoxon = numeric(0))
                                 private$.centralMoments_org <- tibble::tibble(feature = character(0),
                                                                               m1.mean = numeric(0),
                                                                               m2.variance = numeric(0),
                                                                               m3.skewness = numeric(0),
                                                                               m4.kurtosis = numeric(0))
                                 private$.centralMoments_imp <- tibble::tibble(feature = character(0),
                                                                               m1.mean = numeric(0),
                                                                               m2.variance = numeric(0),
                                                                               m3.skewness = numeric(0),
                                                                               m4.kurtosis = numeric(0))
                                 private$.centralMoments_delta <- tibble::tibble(feature = character(0),
                                                                                 d1.mean = numeric(0),
                                                                                 d2.variance = numeric(0),
                                                                                 d3.skewness = numeric(0),
                                                                                 d4.kurtosis = numeric(0))
                                 private$.features <- character(0)
                               }, #resetValidator

                               #' @description
                               #' Performs a comparison between the original and the imputated distribution of a given feature
                               #' using a two-sided Kolmorogow-Smirnow test with simulated p-vaue distribution.
                               #' @param org
                               #' Original data to be analzed.
                               #' (numeric)
                               #' @param imp
                               #' Imputed data to be analyzed.
                               #' (numeric)
                               #' @param feature
                               #' Feature name of the analyzed distributions.
                               #' (character)
                               #' @return
                               #' One row dataframe comprising the test results.
                               #' (tibble::tibble)
                               #' @examples
                               #' x$kolmogorowTestFeature(org, imp, feature)
                               kolmogorowTestFeature = function(org = "numeric", imp = "numeric", feature = "character"){
                                 test_obj <- stats::ks.test(x = org,
                                                            y = imp,
                                                            alternative = "two.sided",
                                                            simulate.p.value=TRUE,
                                                            B=2000)
                                 print(test_obj)
                                 tibble::tibble(feature = c(feature),
                                                d.Kolmogorow = c(test_obj$statistic),
                                                p.Kolmogorow = c(test_obj$p.value)) %>%
                                   return()
                               }, #kolmogorovTestFeature

                               #' @description
                               #' Performs a comparison between the original and the imputated distribution of a given feature
                               #' using a two-sided Wilcoxon/Mann-Whitney test.
                               #' @param org
                               #' Original data to be analzed.
                               #' (numeric)
                               #' @param imp
                               #' Imputed data to be analyzed.
                               #' (numeric)
                               #' @param feature
                               #' Feature name of the analyzed distributions.
                               #' (character)
                               #' @return
                               #' One row dataframe comprising the test results.
                               #' (tibble::tibble)
                               #' @examples
                               #' x$wilcoxonTestFeature(org, imp, feature)
                               wilcoxonTestFeature = function(org = "numeric", imp = "numeric", feature = "character"){
                                 test_obj <- stats::wilcox.test(x = org,
                                                                y = imp,
                                                                alternative = "two.sided")
                                 print(test_obj)
                                 tibble::tibble(feature = c(feature),
                                                w.Wilcoxon = c(test_obj$statistic),
                                                p.Wilcoxon = c(test_obj$p.value)) %>%
                                   return()
                               }, #wilcoxonTestFeature

                               #' @description
                               #' Estimates estimates the central moments of the given distribution.
                               #' @param values
                               #' Data to be analzed.
                               #' (numeric)
                               #' @param feature
                               #' Feature name of the analyzed distributions.
                               #' (character)
                               #' @return
                               #' One row dataframe comprising the statistics.
                               #' (tibble::tibble)
                               #' @examples
                               #' x$centralMomentsFeature(values, feature)
                               centralMomentsFeature = function(values = "numeric",feature = "character"){
                                 cMoment_1 <- mean(values, na.rm = TRUE)
                                 cMoment_2 <- sd(values, na.rm = TRUE)^2
                                 cMoment_3 <- e1071::skewness(values, na.rm = TRUE)
                                 cMoment_4 <- e1071::kurtosis(values, na.rm = TRUE)

                                 tibble::tibble(feature = feature,
                                                m1.mean = c(cMoment_1),
                                                m2.variance = c(cMoment_2),
                                                m3.skewness = c(cMoment_3),
                                                m4.kurtosis = c(cMoment_4)) %>%
                                   return()
                               }, #statisticsFeature

                               #' @description
                               #' Validates the feature distributions of the original and the imputated dataframe``
                               #' using a two-sided Kolmorogow-Smirnow test and a two-sided Wilcoxon/Mann-Whitney test.
                               #' The result is stored in the instance variables `testStatistics_df`and `distributionStatistics_df`.
                               #' Displays the progress if shiny is loaded.
                               #' @param org_df
                               #' Original dataframe to be analzed.
                               #' (tibble::tibble)
                               #' @param imp_df
                               #' Imputed dataframe to be analyzed.
                               #' (tibble::tibble)
                               #' @param progress
                               #' If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
                               #' (shiny::Progress)
                               #' @examples
                               #' x$validate(org, imp, progress)
                               validate = function(org_df = "tbl_df", imp_df = "tbl_df", progress = "Progress"){
                                 self$resetValidator()
                                 set.seed(self$seed)
                                 private$.features <- imp_df %>%
                                   dplyr::select_if(is.numeric) %>%
                                   colnames()
                                 for(feature in self$features){
                                   if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                     progress$inc(1.0/length(self$features))
                                   }#if
                                   org_temp <- org_df %>%
                                     dplyr::select(feature) %>%
                                     tidyr::drop_na() %>%
                                     dplyr::pull(feature)
                                   imp_temp <- imp_df %>%
                                     dplyr::select(feature) %>%
                                     tidyr::drop_na() %>%
                                     dplyr::pull(feature)
                                   kolmogorow_df <- self$kolmogorowTestFeature(org_temp, imp_temp, feature)
                                   wilcoxon_df <- self$wilcoxonTestFeature(org_temp, imp_temp, feature)
                                   test_df <- kolmogorow_df %>%
                                     dplyr::right_join(wilcoxon_df, by = "feature")
                                   private$.testStatistics_df <- self$testStatistics_df %>%
                                     tibble::add_row(test_df)

                                   private$.centralMoments_org <- private$.centralMoments_org %>%
                                     tibble::add_row(self$centralMomentsFeature(org_temp, feature))

                                   private$.centralMoments_imp <- private$.centralMoments_imp %>%
                                     tibble::add_row(self$centralMomentsFeature(imp_temp, feature))
                                 }#for
                                 private$.centralMoments_delta <- tibble::tibble(feature = self$features,
                                                                                 d1.mean = abs(self$centralMoments_org[["m1.mean"]] - self$centralMoments_imp[["m1.mean"]]),
                                                                                 d2.variance = abs(self$centralMoments_org[["m2.variance"]] - self$centralMoments_imp[["m2.variance"]]),
                                                                                 d3.skewness = abs(self$centralMoments_org[["m3.skewness"]] - self$centralMoments_imp[["m3.skewness"]]),
                                                                                 d4.kurtosis = abs(self$centralMoments_org[["m4.kurtosis"]] - self$centralMoments_imp[["m4.kurtosis"]]))
                               }, #validate

                               # ####################
                               # # output functions #
                               # ####################
                               featurePdf = function(org = "numeric", imp = "numeric", feature = "character"){

                               }

                               featurePlot = function(org_df = "tbl_df", imp_df = "tbl_df", feature = "character"){
                                 org <- org_df %>%
                                   dplyr::pull(feature) %>%
                                   as.numeric()
                                 imp <- imp_df %>%
                                   dplyr::pull(feature) %>%
                                   as.numeric()
                                 p1 <- self$featurePdf(org, imp, feature)

                                 return(p1)
                               }

                             )#public
)# class