% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pguLimitsOfQuantification.R
\name{pgu.limitsOfQuantification}
\alias{pgu.limitsOfQuantification}
\title{pgu.limitsOfQuantification}
\format{
\link[R6:R6Class]{R6::R6Class} object.
}
\description{
Handles values in the pguIMP dataset that exceed the limits of quantification.
This object is used by the shiny based gui and is not for use in individual R-scripts!
}
\details{
more information
}
\author{
Sebastian Malkusch, \email{malkusch@med.uni-frankfurt.de}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{loq}}{Returns the instance variable loq
(tibble::tibble)}

\item{\code{setLoq}}{Sets the instance variable loq.
(tibble::tibble)}

\item{\code{outliers}}{Returns instance variable outliers
(tibble::tibble)}

\item{\code{lloqSubstituteAlphabet}}{Returns the instance variable lloqSubstititeAlphabet
(character)}

\item{\code{lloqSubstituteAgent}}{Returns the instance variable lloqSubstituteAgent
(character)}

\item{\code{setLloqSubstituteAgent}}{Sets the instance variable lloqSubstituteAgent.
(character)}

\item{\code{uloqSubstituteAlphabet}}{Returns the instance variable uloqSubstititeAlphabet
(character)}

\item{\code{uloqSubstituteAgent}}{Returns the instance variable uloqSubstituteAgent
(character)}

\item{\code{setUloqSubstituteAgent}}{Sets the instance variable uloqSubstituteAgent.
(character)}

\item{\code{naHandlingAlphabet}}{Returns the instance variable naHandlingAlphabet
(character)}

\item{\code{naHandlingAgent}}{Returns the instance variable naHandlingAgentt
(character)}

\item{\code{setNaHandlingAgent}}{Sets the instance variable naHandlingAgentt
(character)}

\item{\code{loqStatistics}}{Returns the instance variable loqStatistics}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{pgu.limitsOfQuantification$new()}}
\item \href{#method-print}{\code{pgu.limitsOfQuantification$print()}}
\item \href{#method-reset}{\code{pgu.limitsOfQuantification$reset()}}
\item \href{#method-fit}{\code{pgu.limitsOfQuantification$fit()}}
\item \href{#method-predict}{\code{pgu.limitsOfQuantification$predict()}}
\item \href{#method-attribute_lloq}{\code{pgu.limitsOfQuantification$attribute_lloq()}}
\item \href{#method-attribute_uloq}{\code{pgu.limitsOfQuantification$attribute_uloq()}}
\item \href{#method-set_attribute_lloq}{\code{pgu.limitsOfQuantification$set_attribute_lloq()}}
\item \href{#method-set_attribute_uloq}{\code{pgu.limitsOfQuantification$set_attribute_uloq()}}
\item \href{#method-attribute_outliers}{\code{pgu.limitsOfQuantification$attribute_outliers()}}
\item \href{#method-plot_loq_distribution}{\code{pgu.limitsOfQuantification$plot_loq_distribution()}}
\item \href{#method-attribute_bar_plot}{\code{pgu.limitsOfQuantification$attribute_bar_plot()}}
\item \href{#method-attribute_box_plot_with_subset}{\code{pgu.limitsOfQuantification$attribute_box_plot_with_subset()}}
\item \href{#method-attribute_plot}{\code{pgu.limitsOfQuantification$attribute_plot()}}
\item \href{#method-clone}{\code{pgu.limitsOfQuantification$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Mutates outlier candidates characterized as below LLOQ based on user defined actions.


Mutates outlier candidates characterized as above ULOQ based on user defined actions.


Searches for outliers in the given data frame.
If an outlier was found, it is appended to the instance variable outliers.
Indicates if an outlier was found.


Extends the instance variable outliers by one entry.


Tests if the provided attributes are known to the class.


Resets the class' instance variable outliers


Calculates statistics of outlier appearance.
Stores it into the instance variable loqStatistics


Resets the class' instance variable loqStatistics


Resets the class by a data frame comprising information about LOQs.


Resets the class by a vector of attribute names.
The Attributes' LOQs are initially assigned to na.


Clears the heap and
indicates that instance of \code{pgu.limitaOfQuantification} is removed from heap.


Creates and returns a new \code{pgu.limitsOfQuantification} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$new(attribute_names = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attribute_names}}{Vector of attribute names with to be analyzed by the loq object.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{pgu.limitsOfQuantification} object.
(pguIMP::pgu.limitsOfQuantification)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Prints instance variables of a \code{pgu.limitsOfQuantification} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-reset"></a>}}
\if{latex}{\out{\hypertarget{method-reset}{}}}
\subsection{Method \code{reset()}}{
Resets the pguIMP::pgu.limitsOfQuantification object
on the given parameters attribute_names and data_df
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$reset(
  attribute_names = "character",
  data_df = "tbl_df"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attribute_names}}{Vector of attribute names with to be analyzed by the loq object.
(character)}

\item{\code{data_df}}{Dataframe comprising loq information.
Feature names need to be 'attribute', 'LLOQ' and 'ULOQ'.
(tibble::tibble)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fit"></a>}}
\if{latex}{\out{\hypertarget{method-fit}{}}}
\subsection{Method \code{fit()}}{
Analyses the data dets for instances outside of the LOQ defined value interval.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$fit(data_df = "tbl_df")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_df}}{Dataframe to be analyzed}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-predict"></a>}}
\if{latex}{\out{\hypertarget{method-predict}{}}}
\subsection{Method \code{predict()}}{
Mutates all outlier candidates based on user defined actions.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$predict(data_df = "tbl_df")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_df}}{The data to be analyzed.
(tibble::tibble)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The revised data frame
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-attribute_lloq"></a>}}
\if{latex}{\out{\hypertarget{method-attribute_lloq}{}}}
\subsection{Method \code{attribute_lloq()}}{
Returns the attribute's specific lloq.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$attribute_lloq(attribute = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attribute}}{The attribute to be analyzed
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The attribute's lloq
(numeric)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-attribute_uloq"></a>}}
\if{latex}{\out{\hypertarget{method-attribute_uloq}{}}}
\subsection{Method \code{attribute_uloq()}}{
Returns the attribute's specific uloq.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$attribute_uloq(attribute = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attribute}}{The attribute to be analyzed
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The attribute's uloq
(numeric)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_attribute_lloq"></a>}}
\if{latex}{\out{\hypertarget{method-set_attribute_lloq}{}}}
\subsection{Method \code{set_attribute_lloq()}}{
sets the attribute's specific lloq to value.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$set_attribute_lloq(
  attribute = "character",
  value = NA
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attribute}}{The attribute to be updated
(character)}

\item{\code{value}}{The value parsed to the attributes lloq
(numeric)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-set_attribute_uloq"></a>}}
\if{latex}{\out{\hypertarget{method-set_attribute_uloq}{}}}
\subsection{Method \code{set_attribute_uloq()}}{
sets the attribute's specific uloq to value.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$set_attribute_uloq(
  attribute = "character",
  value = NA
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attribute}}{The attribute to be updated
(character)}

\item{\code{value}}{The value parsed to the attributes lloq
(numeric)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-attribute_outliers"></a>}}
\if{latex}{\out{\hypertarget{method-attribute_outliers}{}}}
\subsection{Method \code{attribute_outliers()}}{
Returns the detected outliers of a given attribute.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$attribute_outliers(attribute = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attribute}}{The attribute to be analyzed
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The attribute's outliers
(tibble::tibble)\preformatted{                                        ####################
                                        # data information #
                                        ####################
                                        #' @description
                                        #' Gathers and returns class information
                                        dataInformation = function()\{
                                          self$loq \%>\%
                                            dplyr::summarise_all(class) \%>\%
                                            tidyr::gather(variable, class) \%>\%
                                            return()
                                        \}, #function

                                        ####################
                                        # output functions #
                                        ####################
                                        #' @description
                                        #' Merges dfData and dfMetadata and returns a fromatted data table.
                                        #' @param dfData
                                        #' The data to be analyzed.
                                        #' (tibble::tibble)
                                        #' @param dfMetadata
                                        #' The data frame containing metadata.
                                        #' (tibble::tibble)
                                        #' @return
                                        #' A formatted data table
                                        #' (DT::datatable)
                                        loqDataTable = function(dfData = "tbl_df", dfMetadata = "tbl_df")\{
                                          options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                                          t <- NULL
                                          featureNames <- colnames(dfData)
                                          tryCatch(
                                            dfMerge <- dplyr::bind_cols(dfMetadata, dfData),
                                            error = function(e)\{
                                              print("error")
                                              print(e)
                                              dfMerge <- dfData
                                            \}#error
                                          )#tryCatch
                                          if(self$checkValidity(featureNames))\{
                                            t <- dfMerge \%>\%
                                              dplyr::mutate_if(is.numeric, round, 3) \%>\%
                                              DT::datatable(options = list(scrollX = TRUE,
                                                                           scrollY = '350px',
                                                                           paging = FALSE))
                                            for (featureName in featureNames)\{
                                              featureOutlier <- self$outliers \%>\%
                                                dplyr::filter(feature == featureName) \%>\%
                                                dplyr::mutate_if(is.numeric, round, 3)
                                              if (nrow(featureOutlier) > 0)\{
                                                t <- DT::formatStyle(t,
                                                                     featureName,
                                                                     backgroundColor = DT::styleEqual(dfMerge \%>\%
                                                                                                        dplyr::select(!!featureName) \%>\%
                                                                                                        dplyr::slice(featureOutlier[["measurement"]]) \%>\%
                                                                                                        unlist() \%>\%
                                                                                                        round(digits = 3),
                                                                                                      featureOutlier[["color"]]))
                                              \}#if
                                            \}#for
                                          \}#if
                                          return(t)
                                        \}, #function

                                        #' @description
                                        #' Returns a formatted data table with comrising the information of a user defined attribute's outliers.
                                        #' @param obj
                                        #' The data to be analyzed.
                                        #' (tibble::tibble)
                                        #' @param feature
                                        #' The attribute to be analyzed
                                        #' (character)
                                        #' @return
                                        #' A formatted data table
                                        #' (DT::datatable)
                                        loqFeatureTable = function(obj = "tbl_df", feature = "character")\{
                                          options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))
                                          t <- NULL
                                          if(self$checkValidity(feature))\{
                                            featureOutlier <- self$outliers \%>\%
                                              dplyr::filter(feature == !!feature) \%>\%
                                              dplyr::mutate_if(is.numeric, round, 3)

                                            dfFeature <- obj \%>\%
                                              dplyr::mutate_if(is.numeric, round, 3)

                                            print(dfFeature)

                                            t <- dfFeature \%>\%
                                              DT::datatable(options = list(scrollX = TRUE,
                                                                           scrollY = '350px',
                                                                           paging = FALSE))
                                            if (nrow(featureOutlier) > 0)\{
                                              t <- DT::formatStyle(
                                                t,
                                                feature,
                                                backgroundColor = DT::styleEqual(dfFeature \%>\%
                                                                                   dplyr::select(!!feature) \%>\%
                                                                                   dplyr::slice(featureOutlier[["measurement"]]) \%>\%
                                                                                   unlist() \%>\%
                                                                                   round(digits = 3),
                                                                                 featureOutlier[["color"]]))
                                            \}#if
                                          \}#if
                                          return(t)
                                        \}, #function
}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plot_loq_distribution"></a>}}
\if{latex}{\out{\hypertarget{method-plot_loq_distribution}{}}}
\subsection{Method \code{plot_loq_distribution()}}{
Creates a plot of the instance variable loqStatistics.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$plot_loq_distribution()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A plot.
(ggplot2::ggplot)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-attribute_bar_plot"></a>}}
\if{latex}{\out{\hypertarget{method-attribute_bar_plot}{}}}
\subsection{Method \code{attribute_bar_plot()}}{
Creates a bar plot of a user defined attribute's value distribution.
LOQs are indicated as dotted lines
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$attribute_bar_plot(
  data_df = "tbl_df",
  attribute = "character"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_df}}{The data to be analyzed.
(tibble::tibble)}

\item{\code{attribute}}{The attribute to be analyzed
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A bar plot.
(ggplot2::ggplot)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-attribute_box_plot_with_subset"></a>}}
\if{latex}{\out{\hypertarget{method-attribute_box_plot_with_subset}{}}}
\subsection{Method \code{attribute_box_plot_with_subset()}}{
Creates a box plot of a user defined attribute's value distribution.
LOQs are indicated as dotted lines
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$attribute_box_plot_with_subset(
  data_df = "tbl_df",
  attribute = "character"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_df}}{The data to be analyzed.
(tibble::tibble)}

\item{\code{attribute}}{The attribute to be analyzed
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A box plot.
(ggplot2::ggplot)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-attribute_plot"></a>}}
\if{latex}{\out{\hypertarget{method-attribute_plot}{}}}
\subsection{Method \code{attribute_plot()}}{
Creates and returns a composite graphical analysis
of the outlier analysis of a user defined attribute.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$attribute_plot(
  data_df = "tbl_df",
  attribute = "character"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_df}}{The data to be analyzed.
(tibble::tibble)}

\item{\code{attribute}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Composite result plot.
(gridExtra::grid.arrange)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.limitsOfQuantification$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
