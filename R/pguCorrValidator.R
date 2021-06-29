#' @title pgu.corrValidator
#'
#' @description
#' An R6 class that performs pairwise correlation of the features of the original and the imputed data set.
#' The correlation results of both data sets are compared by subtraction.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.corrValidator$new()
#'
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @importFrom dplyr pull select mutate summarise select_if all_of
#' @importFrom tibble tibble as_tibble is_tibble rownames_to_column
#' @importFrom tidyr gather_
#' @importFrom ggplot2 ggplot aes_string geom_abline geom_point ggtitle xlab ylab theme_linedraw theme element_rect geom_bar geom_boxplot geom_jitter geom_hline scale_x_continuous scale_y_continuous layer_scales coord_flip
#' @importFrom gridExtra grid.arrange
#' @importFrom Hmisc rcorr
#'
#' @examples
#' require(dplyr)
#' require(tibble)
#' data(iris)
#' data_df <- iris %>%
#'   tibble::as_tibble()
#' comp_df <- data_df %>%
#'   dplyr::mutate(Sepal.Length = sample(Sepal.Length))
#' corr_obj = pguIMP::pgu.corrValidator$new()
#' corr_obj$fit(data_df, comp_df)
#' print(corr_obj)
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'


pgu.corrValidator <- R6::R6Class("pgu.corrValidator",
                                 ####################
                                 # instance variables
                                 ####################
                                 private = list(
                                   .featureNames = "character",
                                   .orgR_mat = "matrix",
                                   .impR_mat = "matrix",
                                   .orgP_mat = "matrix",
                                   .impP_mat = "matrix",
                                   .corr_df = "tbl_df",
                                   .summary_df = "tbl_df",

                                   #' @description
                                   #' Clears the heap and
                                   #' indicates if instance of `pgu.corrValidator` is removed from heap.
                                   finalize = function()
                                   {
                                     print("Instance of pgu.corrValidator removed from heap")
                                   },

                                   #' @description
                                   #' Summary of the correlation deviation distribution.
                                   calc_summary = function()
                                   {
                                     if(nrow(self$corr_df)>1)
                                     {
                                       test_result <- self$corr_df %>%
                                         dplyr::pull(cor_delta) %>%
                                         t.test(mu = 0, alternative = "two.sided")

                                       summary_df <- self$corr_df %>%
                                         dplyr::select(cor_delta) %>%
                                         dplyr::summarise(
                                           min = min(cor_delta),
                                           q25 = quantile(cor_delta, probs = c(0.25)),
                                           mu = mean(cor_delta),
                                           median = median(cor_delta),
                                           sigma = sd(cor_delta),
                                           q75 = quantile(cor_delta, probs = c(0.75)),
                                           max = max(cor_delta)) %>%
                                         dplyr::mutate(t.statistic = test_result$statistic) %>%
                                         dplyr::mutate(p.Value = test_result$p.value) %>%
                                         t() %>%
                                         as.data.frame() %>%
                                         tibble::rownames_to_column() %>%
                                         tibble::as_tibble()

                                       colnames(summary_df) <- c("statistics", "values")

                                       private$.summary_df <- summary_df
                                     }

                                   }, #function

                                   #' @description
                                   #' Creates a square matrix which dimension corresponds to the length
                                   #' of the instance variable featureNames. The matrix entries are set to a distinct `value`.
                                   reset_matrix = function(value = "numeric")
                                   {
                                     n = length(self$featureNames)
                                     df <- matrix(data = value,
                                                  nrow = n,
                                                  ncol = n,
                                                  dimnames = list(self$featureNames, self$featureNames))
                                     return(df)
                                   },#function

                                   #' @description
                                   #' Flattens the results transforms them into a dataframe
                                   #' and stores it into the instance variable corr_df.
                                   flatten_matrix = function()
                                   {
                                     ut <- upper.tri(self$orgR_mat)
                                     private$.corr_df <- tibble::tibble(
                                       row = rownames(self$orgR_mat)[row(self$orgR_mat)[ut]],
                                       column = rownames(self$orgR_mat)[col(self$orgR_mat)[ut]],
                                       cor_org  = self$orgR_mat[ut],
                                       p_org = self$orgP_mat[ut],
                                       cor_imp  = self$impR_mat[ut],
                                       p_imp = self$impP_mat[ut]
                                     ) %>%
                                       dplyr::mutate(cor_delta = cor_imp - cor_org)
                                   } #function

                                 ), #private
                                 ##################
                                 # accessor methods
                                 ##################
                                 active = list(
                                   #' @field featureNames
                                   #' Returns the instance variable featureNames.
                                   #' (character)
                                   featureNames = function(){
                                     return(private$.featureNames)
                                   },
                                   #' @field orgR_mat
                                   #' Returns the instance variable orgR_mat.
                                   #' (matrix)
                                   orgR_mat = function(){
                                     return(private$.orgR_mat)
                                   },
                                   #' @field impR_mat
                                   #' Returns the instance variable impR_mat.
                                   #' (matrix)
                                   impR_mat = function(){
                                     return(private$.impR_mat)
                                   },
                                   #' @field orgP_mat
                                   #' Returns the instance variable orgP_mat.
                                   #' (matrix)
                                   orgP_mat = function(){
                                     return(private$.orgP_mat)
                                   },
                                   #' @field impP_mat
                                   #' Returns the instance variable impP_mat.
                                   #' (matrix)
                                   impP_mat = function(){
                                     return(private$.impP_mat)
                                   },
                                   #' @field corr_df
                                   #' Returns the instance variable corr_df.
                                   #' (tibble::tibble)
                                   corr_df = function(){
                                     return(private$.corr_df)
                                   },
                                   #' @field summary_df
                                   #' Returns the instance variable summary_df.
                                   #' (tibble::tibble)
                                   summary_df = function(){
                                     return(private$.summary_df)
                                   }
                                 ), #active
                                 ###################
                                 # memory management
                                 ###################
                                 public = list(
                                   #' @description
                                   #' Creates and returns a new `pgu.corrValidator` object.
                                   #' @param org_df
                                   #' The original data to be analyzed.
                                   #' (tibble::tibble)
                                   #' @param imp_df
                                   #' The imputed version of the org_df data.
                                   #' @return
                                   #' A new `pgu.corrValidator` object.
                                   #' (pguIMP::pgu.corrValidator)
                                   initialize = function(org_df = "tbl_df", imp_df = "tbl_df"){
                                     self$reset()
                                   },

                                   #' @description
                                   #' Prints instance variables of a `pgu.corrValidator` object.
                                   #' @return
                                   #' string
                                   print = function(){
                                     rString <- sprintf("\npgu.correlator\n")
                                     cat(rString)
                                     self$corr_df %>%
                                       print()
                                     cat("\n\n")
                                     invisible(self)
                                   }, #print
                                   #####################
                                   # analyze functions #
                                   #####################
                                   #' @description
                                   #' Resets the object `pgu.corrValidator` based on the instance variable featureNames..
                                   reset = function(){
                                     private$.orgR_mat <- private$reset_matrix(value = 0)
                                     private$.impR_mat <- private$reset_matrix(value = 0)
                                     private$.orgP_mat <- private$reset_matrix(value = 1)
                                     private$.impP_mat <- private$reset_matrix(value = 1)
                                     private$flatten_matrix()
                                     #private$calc_summary()
                                   },

                                   #' @description
                                   #' Runs the corraltion analysis.
                                   #' @param org_df
                                   #' Adataframe comprising the original data.
                                   #' (tibble::tibble)
                                   #' @param imp_df
                                   #' Adataframe comprising the imputed data.
                                   #' (tibble::tibble)
                                   fit = function(org_df = "tbl_df", imp_df = "tbl_df")
                                     {
                                     if((tibble::is_tibble(org_df)) & (tibble::is_tibble(imp_df)))
                                       {
                                       private$.featureNames <- org_df %>%
                                         dplyr::select_if(is.numeric) %>%
                                         colnames()

                                       if((nrow(org_df) > 4) & (nrow(imp_df) > 4)){
                                         res_org <- org_df %>%
                                           dplyr::select(dplyr::all_of(self$featureNames)) %>%
                                           as.matrix() %>%
                                           Hmisc::rcorr(type = "pearson")

                                         private$.orgR_mat <- res_org$r
                                         private$.orgP_mat <- res_org$P

                                         res_imp <- imp_df %>%
                                           dplyr::select(dplyr::all_of(self$featureNames)) %>%
                                           as.matrix() %>%
                                           Hmisc::rcorr(type = "pearson")

                                         private$.impR_mat <- res_imp$r
                                         private$.impP_mat <- res_imp$P

                                         private$flatten_matrix()
                                         private$calc_summary()
                                       }
                                       else{
                                         self$reset()
                                       }
                                     }
                                     else {
                                       self$reset()
                                     }


                                   }, #validate

                                   #' @description
                                   #' Plots the correlation analysis results.
                                   correlationScatterPlot = function(){
                                     p <- self$corr_df %>%
                                       dplyr::mutate(pair = paste(row, column, sep = "/\n")) %>%
                                       dplyr::select(c("pair", "cor_org", "cor_imp")) %>%
                                       ggplot2::ggplot() +
                                       ggplot2::geom_point(mapping = ggplot2::aes_string(x="cor_org", y="cor_imp", color = "pair")) +
                                       ggplot2::geom_abline(mapping = NULL, data = NULL, slope = 1, intercept = 0, show.legend = NA, linetype = "dashed") +
                                       # ggplot2::xlim(-1,1) +
                                       # ggplot2::ylim(-1,1) +
                                       ggplot2::ggtitle("Correlation Plot") +
                                       ggplot2::xlab("r (original data)") +
                                       ggplot2::ylab("r (imputed data)") +
                                       ggplot2::theme_linedraw() +
                                       ggplot2::theme(
                                         panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                         plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                         legend.position = "none"
                                       )
                                     return(p)
                                   },#plot

                                   #' @description
                                   #' Creates and returns a histogram from the cor_delat values.
                                   #' @return
                                   #' Bar plot
                                   #' (ggplot2::ggplot)
                                   correlationBarPlot = function(){
                                     p <- self$corr_df %>%
                                       dplyr::select(c("cor_delta")) %>%
                                       ggplot2::ggplot(mapping = ggplot2::aes_string(x = "cor_delta"), na.rm=TRUE) +
                                       ggplot2::geom_bar(stat = "bin", bins = 30) +
                                       ggplot2::ylab("counts") +
                                       ggplot2::xlab("value") +
                                       ggplot2::theme_linedraw() +
                                       ggplot2::theme(
                                         panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                         plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                         legend.background = ggplot2::element_rect(fill = "transparent"),
                                         legend.key = ggplot2::element_rect(fill = "transparent")
                                       )
                                     return(p)
                                   }, #function

                                   #' @description
                                   #' Plots the correlation analysis results.
                                   correlationBoxPlot = function(){
                                     p <- self$corr_df %>%
                                       dplyr::select(c("cor_delta")) %>%
                                       tidyr::gather_(key="name", value="measurement", "cor_delta") %>%
                                       ggplot2::ggplot(mapping=ggplot2::aes_string(x="name",y="measurement"), na.rm=TRUE)+
                                       ggplot2::geom_boxplot(na.rm=TRUE, outlier.shape = NA)+
                                       ggplot2::geom_jitter(color = "darkblue", na.rm=TRUE) +
                                       ggplot2::geom_hline(yintercept = 0.0, linetype = "dashed") +
                                       ggplot2::ylab("r(imp) - r(org)") +
                                       ggplot2::xlab("delta(r)") +
                                       ggplot2::theme_linedraw() +
                                       ggplot2::theme(
                                         panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                         plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                         legend.background = ggplot2::element_rect(fill = "transparent"),
                                         legend.key = ggplot2::element_rect(fill = "transparent")
                                       )
                                     return(p)
                                   }, #function

                                   #' @description
                                   #' Creates and returns a compund graphical analysis of the cor_delta values.
                                   #' @return
                                   #' Compound plot
                                   #' (gridExtra::grid.arrange)
                                   correlationCompoundPlot = function(){
                                       p1 <- self$correlationBoxPlot()
                                       p2 <- self$correlationBarPlot() +
                                         ggplot2::scale_x_continuous(position = "top") +
                                         ggplot2::coord_flip()
                                       limits <- ggplot2::layer_scales(p2)$x$range$range
                                       p1 <- p1 +
                                         ggplot2::scale_y_continuous(limits = limits)



                                       p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)),
                                                                    top = textGrob(label = "Distribution of r(imp) - r(org)"))
                                     return(p)
                                   } #function
                                 )#public

)# class
