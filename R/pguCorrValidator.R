#' @title pgu.corrValidator
#'
#' @description
#' A class that performs pairwise correlation on the pguIMP data set.
#'
#' @details
#'
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.correlator$new(data)
#'
#' @import R6
#' @import tidyverse
#' @import Hmisc
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
                                   .corr_df = "tbl_df"
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
                                   #' @examples
                                   #' x <- tibble::tibble()
                                   #' y <- impute(x)
                                   #' obj <- pguIMP::pgu.corrValidator$new(org_df = x, imp_df = y)
                                   initialize = function(org_df = "tbl_df", imp_df = "tbl_df"){
                                     self$validate(org_df, imp_df)
                                   },
                                   #' @description
                                   #' Clears the heap and
                                   #' indicates if instance of `pgu.corrValidator` is removed from heap.
                                   finalize = function(){
                                     print("Instance of pgu.corrValidator removed from heap")
                                   },
                                   #' @description
                                   #' Prints instance variables of a `pgu.corrValidator` object.
                                   #' @return
                                   #' string
                                   #' @examples
                                   #' pgu.corrValidator$print()
                                   #' print(pgu.corrValidator)
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
                                   #' @examples
                                   #' pgu.corrValidator$reset()
                                   reset = function(){
                                     private$.orgR_mat <- self$resetMatrix(value = 0)
                                     private$.impR_mat <- self$resetMatrix(value = 0)
                                     private$.orgP_mat <- self$resetMatrix(value = 1)
                                     private$.impP_mat <- self$resetMatrix(value = 1)
                                     self$flattenMatrix()
                                   },
                                   #' @description
                                   #' Creates a square matrix which dimension corresponds to the length
                                   #' of the instance variable featureNames. The matrix entries are set to a distinct `value`.
                                   #' @param value
                                   #' The value the matrix entries are set to.
                                   #' (numeric)
                                   #' @return
                                   #' A square matrix.
                                   #' (matrix)
                                   #' @examples
                                   #' matrix <- pgu.corrValidator$resetMatrix(value)
                                   resetMatrix = function(value = "numeric"){
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
                                   #' @examples
                                   #' pgu.corrValidatior$flattenMatrix()
                                   flattenMatrix = function() {
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
                                   }, #function
                                   #' @description
                                   #' Runs the corraltion analysis.
                                   #' @param org_df
                                   #' Adataframe comprising the original data.
                                   #' (tibble::tibble)
                                   #' @param imp_df
                                   #' Adataframe comprising the imputed data.
                                   #' (tibble::tibble)
                                   #' @examples
                                   #' pgu.corrValidator$validate(org_df, imp_df)
                                   validate = function(org_df = "tbl_df", imp_df = "tbl_df"){
                                     if(!tibble::is_tibble(org_df)){
                                       org_df <- tibble::tibble(names = c("none"),
                                                                values = as.numeric(c(NA)))
                                     }

                                     private$.featureNames <- org_df %>%
                                       dplyr::select_if(is.numeric) %>%
                                       colnames()

                                     if(nrow(org_df) > 4){
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

                                       self$flattenMatrix()
                                     }
                                     else{
                                       self$reset()
                                     }
                                   }, #validate

                                   #' @description
                                   #' Summary of the correlation deviation distribution.
                                   #' @return
                                   #' A dataframe comprising the summary of the correlation deviation distribution.
                                   #' (tibble::tibble)
                                   #' @examples
                                   #' df <- pgu.corrValidator$summary()
                                   summary = function(){
                                     test_result <- self$corr_df %>%
                                       dplyr::pull(cor_delta) %>%
                                       t.test(mu = 0, alternative = "two.sided")

                                     summary_df <- self$corr_df %>%
                                       dplyr::select(cor_delta) %>%
                                       dplyr::summarise(tibble(min = min(cor_delta),
                                                               q25 = quantile(cor_delta, probs = c(0.25)),
                                                               mu = mean(cor_delta),
                                                               median = median(cor_delta),
                                                               sigma = sd(cor_delta),
                                                               q75 = quantile(cor_delta, probs = c(0.75)),
                                                               max = max(cor_delta))) %>%
                                       dplyr::mutate(t.statistic = test_result$statistic) %>%
                                       dplyr::mutate(p.Value = test_result$p.value) %>%
                                       t() %>%
                                       as.data.frame() %>%
                                       tibble::rownames_to_column() %>%
                                       tibble::as_tibble()

                                     colnames(summary_df) <- c("statistics", "values")

                                     return(summary_df)
                                   }, #function

                                   #' @description
                                   #' Plots the correlation analysis results.
                                   #' @examples
                                   #' pgu.corrValidator$correlationScatterPlot()
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
                                   #' @examples
                                   #' x$correlationBarPlot() %>%
                                   #'  show()
                                   correlationBarPlot = function(){
                                     p <- self$corr_df %>%
                                       dplyr::select(c("cor_delta")) %>%
                                       ggplot2::ggplot(mapping = ggplot2::aes_string(x = "cor_delta"), na.rm=TRUE) +
                                       ggplot2::geom_bar(stat = "bin") +
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
                                   #' @examples
                                   #' pgu.corrValidator$correlationBoxPlot()
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
                                   #' @examples
                                   #' x$correlationCompoundPlot() %>%
                                   #'  show()
                                   correlationCompoundPlot = function(){
                                       p1 <- self$correlationBoxPlot()
                                       limits <- ggplot2::layer_scales(p1)$y$range$range
                                       p2 <- self$correlationBarPlot() +
                                         ggplot2::scale_x_continuous(position = "top", limits=limits) +
                                         ggplot2::coord_flip()
                                       p <- gridExtra::grid.arrange(p1,p2, layout_matrix = rbind(c(1,2),c(1,2)),
                                                                    top = textGrob(label = "Distribution of r(imp) - r(org)"))
                                     return(p)
                                   } #function
                                 )#public

)# class
