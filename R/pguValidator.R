#' @title pgu.validator
#'
#' @description
#' Validates that the distribution is not significantly altered by the imputation process.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @details
#' Takes two distributions (before and after imputation).
#' Performs a Wilcoxon-Mann-Whitney U test.
#' Performs a Kolmogorow-Smirnow test.
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom DataVisualizations ParetoDensityEstimation
#' @importFrom dplyr all_of filter mutate pull rename right_join select select_if
#' @importFrom e1071 kurtosis skewness
#' @importFrom ggplot2 aes aes_string element_rect geom_abline geom_boxplot geom_hline geom_jitter
#' @importFrom ggplot2 geom_line geom_point ggplot ggtitle theme theme_linedraw
#' @importFrom ggplot2 xlab ylab
#' @importFrom grid gpar textGrob
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr %>%
#' @importFrom purrr discard
#' @importFrom R6 R6Class
#' @importFrom shiny Progress
#' @importFrom stats ks.test wilcox.test
#' @importFrom tibble add_row as_tibble tibble
#' @importFrom tidyr drop_na gather
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
                               kolmogorowTestFeature = function(org = "numeric", imp = "numeric", feature = "character"){
                                 test_obj <- stats::ks.test(x = org,
                                                            y = imp,
                                                            alternative = "two.sided",
                                                            simulate.p.value=TRUE,
                                                            B=2000,
                                                            exact = FALSE)

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
                               wilcoxonTestFeature = function(org = "numeric", imp = "numeric", feature = "character"){
                                 test_obj <- stats::wilcox.test(x = org,
                                                                y = imp,
                                                                alternative = "two.sided")
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
#
                               # ####################
                               # # output functions #
                               # ####################

                               # @description
                               # Receives a dataframe and plost the feature 'x' against the features 'org_pdf' and 'imp_pdf'.
                               # Returns the plot
                               # @param data_df
                               # dataframe to be plotted
                               # (tibble::tibble)
                               # @return
                               # A ggplot2 object
                               # (ggplot2::ggplot)
                               # featurePdf = function(data_df = "tbl_df"){
                               #   title_str <- sprintf("probability density")
                               #   p <- data_df %>%
                               #     dplyr::select(c("x", "org_pdf", "imp_pdf")) %>%
                               #     dplyr::rename(original = org_pdf) %>%
                               #     dplyr::rename(imputed = imp_pdf) %>%
                               #     tidyr::gather(key = "type", value = "pdf", -x, na.rm = TRUE) %>%
                               #     ggplot2::ggplot() +
                               #     ggplot2::geom_line(mapping = ggplot2::aes_string(x="x", y="pdf", color="type", linetype = "type"), size = 1.5) +
                               #     ggplot2::ggtitle(title_str) +
                               #     ggplot2::xlab("value")
                               #   return(p)
                               # }, #featurePdf

                               #' @description
                               #' Receives a dataframe and plots the pareto density of the features 'org_pdf' and 'imp_pdf'.
                               #' Returns the plot
                               #' @param data_df
                               #' dataframe to be plotted
                               #' (tibble::tibble)
                               #' @return
                               #' A ggplot2 object
                               #' (ggplot2::ggplot)
                               featurePdf = function(data_df = "tbl_df"){
                                 p <- data_df %>%
                                   ggplot2::ggplot() +
                                   ggplot2::geom_line(mapping = ggplot2::aes_string(x="value", y = "pde", color = "data", linetype = "data"), size = 1.5) +
                                   ggplot2::ylab("pdf") +
                                   ggplot2::theme_linedraw() +
                                   ggplot2::theme(
                                     panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                     plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                     legend.background = ggplot2::element_rect(fill = "transparent"),
                                     legend.key = ggplot2::element_rect(fill = "transparent")
                                   )
                                 return(p)
                               }, #featurePdf

                               #' @description
                               #' Receives a dataframe and plost the feature 'x' against the features 'org_cdf' and 'imp_cdf'.
                               #' Returns the plot
                               #' @param data_df
                               #' dataframe to be plotted
                               #' (tibble::tibble)
                               #' @return
                               #' A ggplot2 object
                               #' (ggplot2::ggplot)
                               featureCdf = function(data_df = "tbl_df"){
                                 title_str <- sprintf("cumulative density")
                                 p <- data_df %>%
                                   dplyr::select(c("x", "org_cdf", "imp_cdf")) %>%
                                   dplyr::rename(original = org_cdf) %>%
                                   dplyr::rename(imputed = imp_cdf) %>%
                                   tidyr::gather(key = "type", value = "cdf", -x, na.rm = TRUE) %>%
                                   ggplot2::ggplot() +
                                   ggplot2::geom_line(mapping = ggplot2::aes_string(x="x", y="cdf", color="type",  linetype = "type"), size = 1.5) +
                                   ggplot2::ggtitle(title_str) +
                                   ggplot2::xlab("value") +
                                   ggplot2::theme_linedraw() +
                                   ggplot2::theme(
                                     panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                     plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                     legend.background = ggplot2::element_rect(fill = "transparent"),
                                     legend.key = ggplot2::element_rect(fill = "transparent")
                                   )
                                 return(p)
                               }, #featureCdf

                               #' @description
                               #' Receives two numeric vectors 'org' and 'imp'. Plots the qq-plot of both vectors.
                               #' Returns the plot
                               #' @param org
                               #' Numric vector comprising the original data.
                               #' (numeric)
                               #' @param imp
                               #' Numeric vector comprising the imputed data.
                               #' (numeric)
                               #' @return
                               #' A ggplot2 object
                               #' (ggplot2::ggplot)
                               featureVs = function(org = "numeric", imp = "numeric"){
                                 min_val <- min(min(org), min(imp))
                                 max_val <- max(max(org), max(imp))

                                 p <- tibble::as_tibble(qqplot(org, imp, plot.it=FALSE)) %>%
                                   ggplot2::ggplot() +
                                   ggplot2::geom_point(mapping = ggplot2::aes_string(x="x", y="y")) +
                                   ggplot2::geom_abline(intercept = 0, slope = 1) +
                                   ggplot2::ggtitle("Imputation scatter plot") +
                                   ggplot2::xlab("orgiginal") +
                                   ggplot2::ylab("imputed") +
                                   ggplot2::theme_linedraw() +
                                   ggplot2::theme(
                                     panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                     plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                     legend.background = ggplot2::element_rect(fill = "transparent"),
                                     legend.key = ggplot2::element_rect(fill = "transparent")
                                   )

                                 return(p)
                               }, #featureQQ

                               #' @description
                               #' Receives a dataframe and information about the lloq and uloq and retuns a boxplot
                               #' @param data_df
                               #' Dataframe to be analyzed
                               #' (tibble::tibble)
                               #' @param lloq
                               #' lower limit of quantification
                               #' (numeric)
                               #' @param uloq
                               #' upper limit of quantification
                               #' (numeric)
                               #' @param feature
                               #' Feature name
                               #' (character)
                               #' @return
                               #' A ggplot2 object
                               #' (ggplot2::ggplot)
                               featureBoxPlot = function(data_df = "tbl_df", lloq = "numeric", uloq = "numeric", feature = "character"){
                                 p <- data_df %>%
                                   tidyr::gather(key = !!feature, value="value", -type) %>%
                                   ggplot2::ggplot(mapping=ggplot2::aes_string(x=feature,y="value"), na.rm=TRUE)+
                                   ggplot2::geom_boxplot(na.rm=TRUE, outlier.shape = NA)+
                                   ggplot2::geom_jitter(ggplot2::aes(color = type), na.rm=TRUE) +
                                   ggplot2::geom_hline(yintercept=lloq, linetype="dashed", na.rm = TRUE) +
                                   ggplot2::geom_hline(yintercept=uloq, linetype="dashed", na.rm = TRUE) +
                                   ggplot2::theme_linedraw() +
                                   ggplot2::theme(
                                     panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                     plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                     legend.background = ggplot2::element_rect(fill = "transparent"),
                                     legend.key = ggplot2::element_rect(fill = "transparent")
                                   )
                                 return(p)
                               }, #featureBoxPlot

                               #' @description
                               #' Receives two numeric dataframes 'org_df' and 'imp_df', and a feature name.
                               #' Creates a compund plot of the validation results for the given feature..
                               #' Returns the plot
                               #' @param org_df
                               #' Dataframe comprising the original data.
                               #' (tibble::tibble)
                               #' @param imp_df
                               #' Dataframe comprising the imputed data.
                               #' (tibble::tibble)
                               #' @param lloq
                               #' lower limit of quantification
                               #' (numeric)
                               #' @param uloq
                               #' upper limit of quantification
                               #' (numeric)
                               #' @param impIdx_df
                               #' dataframe comprising information about imputation sites
                               #' (tibble::tibble)
                               #' @param feature
                               #' Feature name.
                               #' (character)
                               #' @return
                               #' A ggplot2 object
                               #' (ggplot2::ggplot)
                               featurePlot = function(org_df = "tbl_df", imp_df = "tbl_df", lloq = "numeric", uloq = "numeric", impIdx_df = "tbl_df", feature = "character"){
                                 p <- NULL
                                 if((feature %in% colnames(org_df)) & (feature %in% colnames(imp_df)))
                                 {

                                   title_str <- sprintf("Imputation quality analysis of %s", feature)

                                   org <- org_df %>%
                                     dplyr::select(dplyr::all_of(feature)) %>%
                                     # tidyr::drop_na() %>%
                                     dplyr::pull(feature) %>%
                                     as.numeric()
                                   imp <- imp_df %>%
                                     dplyr::select(dplyr::all_of(feature)) %>%
                                     # tidyr::drop_na() %>%
                                     dplyr::pull(feature) %>%
                                     as.numeric()

                                   org_hist <- org %>%
                                     purrr::discard(is.na) %>%
                                     hist(plot = FALSE)
                                   org_min <- org_hist$breaks[1]
                                   org_max <- org_hist$breaks[length(org_hist$breaks)]

                                   imp_hist <- imp %>%
                                     purrr::discard(is.na) %>%
                                     hist(plot = FALSE)
                                   imp_min <- imp_hist$breaks[1]
                                   imp_max <- imp_hist$breaks[length(imp_hist$breaks)]

                                   data_min <- min(c(org_min, imp_min))
                                   data_max <- max(c(org_max, imp_max))

                                   org_pdf_obj <- density(org, bw = "nrd0", adjust = 1, kernel = "gaussian",from = data_min, to = data_max, n=512, na.rm=TRUE)
                                   imp_pdf_obj <- density(imp, bw = "nrd0", adjust = 1, kernel = "gaussian",from = data_min, to = data_max, n=512, na.rm=TRUE)

                                   dist_df <- tibble::tibble(x = org_pdf_obj$x,
                                                             org_pdf = org_pdf_obj$y,
                                                             imp_pdf = imp_pdf_obj$y)
                                   dist_df <- dist_df %>%
                                     dplyr::mutate(org_cdf = cumsum(org_pdf)*abs(x[2] - x[1])) %>%
                                     dplyr::mutate(imp_cdf = cumsum(imp_pdf)*abs(x[2] - x[1]))

                                   pde_org <- org_df %>%
                                     dplyr::select(dplyr::all_of(feature)) %>%
                                     tidyr::drop_na() %>%
                                     dplyr::pull(feature) %>%
                                     DataVisualizations::ParetoDensityEstimation()

                                   pde_imp <- imp_df %>%
                                     dplyr::select(dplyr::all_of(feature)) %>%
                                     tidyr::drop_na() %>%
                                     dplyr::pull(feature) %>%
                                     DataVisualizations::ParetoDensityEstimation()

                                   pde_df <- tibble::tibble(value = c(pde_org$kernels, pde_imp$kernels),
                                                            pde = c(pde_org$paretoDensity, pde_imp$paretoDensity),
                                                            data = c(rep("original", times = length(pde_org$kernels)), rep("imputed", times = length(pde_imp$kernels))))


                                   box_df <- tibble::tibble(original = org,
                                                            imputed = imp)

                                   impIdx <- impIdx_df %>%
                                     dplyr::filter(feature == !!feature) %>%
                                     dplyr::pull(idx) %>%
                                     as.integer()

                                   type_vec  <- rep("original", nrow(box_df))
                                   type_vec[impIdx] <- "imputed"

                                   box_df <- box_df %>%
                                     dplyr::mutate(type = type_vec)

                                   p1 <- self$featurePdf(pde_df) +
                                     ggplot2::theme(legend.position = "none")
                                   p2 <- self$featureBoxPlot(data_df = box_df, lloq = lloq, uloq = uloq, feature = feature) +
                                     ggplot2::theme(legend.position = "none")
                                   p3 <- self$featureCdf(dist_df) +
                                     ggplot2::theme(legend.position = "top")
                                   p4 <- self$featureVs(org, imp)
                                   p <- gridExtra::grid.arrange(p1,p2,p3, p4,
                                                                layout_matrix = rbind(c(1,1,2,3),c(1,1,2,4)),
                                                                top = grid::textGrob(title_str, gp = grid::gpar(fontsize=20)))
                                 }
                                 return(p)
                               } #featurePlot

                             )#public
)# class
