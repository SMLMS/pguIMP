% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pguData.R
\name{pgu.data}
\alias{pgu.data}
\title{pgu.data}
\format{
\link[R6:R6Class]{R6::R6Class} object.
}
\description{
Handles the pguIMP dataset.
}
\details{
Stores the pguIMP dataset as instance variable and keeps track of the attributes of interest.
Provides additionally fast access to several statistical information about the data set.
This object is used by the shiny based gui and is not for use in individual R-scripts!
}
\examples{
require(dplyr)
require(tibble)
data(iris)
data_df <- iris \%>\%
  tibble::as_tibble()
data_obj = pguIMP::pgu.data$new(data_df)
}
\author{
Sebastian Malkusch, \email{malkusch@med.uni-frankfurt.de}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{rawData}}{Returns the instance variable rawData
(tibble::tibble)}

\item{\code{setRawData}}{Sets the instance variable rawData
(tibble::tibble)}

\item{\code{attributeNames}}{Returns the instance variable attributeNames
(character)}

\item{\code{numericalAttributeNames}}{Returns the instance variable numericalAttributeNames
(character)}

\item{\code{categoricalAttributeNames}}{Returns the instance variable categoricalAttributeNames
(character)}

\item{\code{classInformation}}{Returns the instance variable classInformation
(tibble::tibble)}

\item{\code{statistics}}{Returns the instance variable statistics
(tibble::tibble)}

\item{\code{reducedStatistics}}{Returns the instance variable reducedStatistics
(tibble::tibble)}

\item{\code{missingsStatistics}}{Returns the instance variable missingsStatistics
(tibble::tibble)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{pgu.data$new()}}
\item \href{#method-print}{\code{pgu.data$print()}}
\item \href{#method-fit}{\code{pgu.data$fit()}}
\item \href{#method-attribute_index}{\code{pgu.data$attribute_index()}}
\item \href{#method-numerical_data}{\code{pgu.data$numerical_data()}}
\item \href{#method-categorical_data}{\code{pgu.data$categorical_data()}}
\item \href{#method-clone}{\code{pgu.data$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Clears the heap and
indicates that instance of pguIMP::pgu.data is removed from heap.


Summarizes information on the instance variable rawData
and retruns it in form of a compact data frame.


Summarizes a vector of numericals and returns summary.


Iterativley calls the function summarize_numerical_data on all numerical attributes
of the instance variable rawData and returns the result in form of a data frame.


Calls the function calculate_statistics
filters the result for the attribute names and arithmetic mean values.
and returns the result in form of a data frame.


Calls the class' function dataStatistics
filters the result for the attribute names and information about missing values.
and returns the result in form of a data frame.


Creates and returns a new pguIMP::pgu.data object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.data$new(data_df = "tbl_df")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_df}}{The data to be analyzed.
(tibble::tibble)}

\item{\code{val}}{Vector of numericals to be summarized.
(numeric)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new pguIMP::pgu.data object.
(pguIMP::pgu.data)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Prints instance variables of a pguIMP::pgu.data object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.data$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fit"></a>}}
\if{latex}{\out{\hypertarget{method-fit}{}}}
\subsection{Method \code{fit()}}{
Extracts information about the instance variable rawData.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.data$fit()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-attribute_index"></a>}}
\if{latex}{\out{\hypertarget{method-attribute_index}{}}}
\subsection{Method \code{attribute_index()}}{
Returns the index of an attribute within the instance variable attributeNames.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.data$attribute_index(attribute = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{attribute}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Index of attribute's name in rawData
(numeric)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-numerical_data"></a>}}
\if{latex}{\out{\hypertarget{method-numerical_data}{}}}
\subsection{Method \code{numerical_data()}}{
Returns the numeric attributes of the instance variable rawData.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.data$numerical_data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A data frame
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-categorical_data"></a>}}
\if{latex}{\out{\hypertarget{method-categorical_data}{}}}
\subsection{Method \code{categorical_data()}}{
Returns the categorical attributes of the instance variable rawData.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.data$categorical_data()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A data frame
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.data$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
