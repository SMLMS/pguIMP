% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pguImporter.R
\name{pgu.importer}
\alias{pgu.importer}
\title{pgu.importer}
\format{
\link[R6:R6Class]{R6::R6Class} object.
}
\description{
Handles the data import
}
\details{
Menages the import of the pguIMP dataset
This object is used by the shiny based gui and is not for use in individual R-scripts!
}
\author{
Sebastian Malkusch, \email{malkusch@med.uni-frankfurt.de}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{suffixes}}{Returns the instance variable suffixes
(character)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{pgu.importer$new()}}
\item \href{#method-finalize}{\code{pgu.importer$finalize()}}
\item \href{#method-print}{\code{pgu.importer$print()}}
\item \href{#method-suffixIsKnown}{\code{pgu.importer$suffixIsKnown()}}
\item \href{#method-importData}{\code{pgu.importer$importData()}}
\item \href{#method-importLoq}{\code{pgu.importer$importLoq()}}
\item \href{#method-importMetadata}{\code{pgu.importer$importMetadata()}}
\item \href{#method-clone}{\code{pgu.importer$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates and returns a new \code{pgu.importer} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.importer$new()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A new \code{pgu.importer} object.
(pguIMP::pgu.importer)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-finalize"></a>}}
\if{latex}{\out{\hypertarget{method-finalize}{}}}
\subsection{Method \code{finalize()}}{
Clears the heap and
indicates that instance of \code{pgu.importer} is removed from heap.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.importer$finalize()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Prints instance variables of a \code{pgu.importer} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.importer$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-suffixIsKnown"></a>}}
\if{latex}{\out{\hypertarget{method-suffixIsKnown}{}}}
\subsection{Method \code{suffixIsKnown()}}{
Takes an instance of pgu.file and tests if the suffix is valid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.importer$suffixIsKnown(obj = "pgu.file")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{obj}}{instance of pgu.file.
(pguIMP::pgu.file)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
test result
(logical)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-importData"></a>}}
\if{latex}{\out{\hypertarget{method-importData}{}}}
\subsection{Method \code{importData()}}{
Takes an instance of pgu.file imports a dataset.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.importer$importData(obj = "pgu.file")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{obj}}{instance of pgu.file.
(pguIMP::pgu.file)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data frame
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-importLoq"></a>}}
\if{latex}{\out{\hypertarget{method-importLoq}{}}}
\subsection{Method \code{importLoq()}}{
Takes an instance of pgu.file imports a loq  dataset.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.importer$importLoq(obj = "pgu.file")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{obj}}{instance of pgu.file.
(pguIMP::pgu.file)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data frame
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-importMetadata"></a>}}
\if{latex}{\out{\hypertarget{method-importMetadata}{}}}
\subsection{Method \code{importMetadata()}}{
Takes an instance of pgu.file imports a metadata dataset.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.importer$importMetadata(obj = "pgu.file")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{obj}}{instance of pgu.file.
(pguIMP::pgu.file)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data frame
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.importer$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
