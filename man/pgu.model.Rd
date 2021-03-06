% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pguModel.R
\name{pgu.model}
\alias{pgu.model}
\title{pgu.model}
\format{
\link[R6:R6Class]{R6::R6Class} object.
}
\description{
Comprises a list of models for data manipulation.
}
\details{
Comprises a list of pgu.normDist objects and model parameters.
These can be used to scale data.
This object is used by the shiny based gui and is not for use in individual R-scripts!
}
\author{
Sebastian Malkusch, \email{malkusch@med.uni-frankfurt.de}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{modelList}}{Returns a vector of pgu-normDist objects.
(pgu.normDist)}

\item{\code{modelParameter}}{Returns a dataframe comrising model parameters.
(tibble::tibble)}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{pgu.model$new()}}
\item \href{#method-finalize}{\code{pgu.model$finalize()}}
\item \href{#method-print}{\code{pgu.model$print()}}
\item \href{#method-resetModelParameter}{\code{pgu.model$resetModelParameter()}}
\item \href{#method-resetModelList}{\code{pgu.model$resetModelList()}}
\item \href{#method-resetModel}{\code{pgu.model$resetModel()}}
\item \href{#method-setNormDist}{\code{pgu.model$setNormDist()}}
\item \href{#method-featureIdx}{\code{pgu.model$featureIdx()}}
\item \href{#method-fitFeature}{\code{pgu.model$fitFeature()}}
\item \href{#method-fitData}{\code{pgu.model$fitData()}}
\item \href{#method-logFitResultsFeature}{\code{pgu.model$logFitResultsFeature()}}
\item \href{#method-logFailedFitResultsFeature}{\code{pgu.model$logFailedFitResultsFeature()}}
\item \href{#method-scaleNumeric}{\code{pgu.model$scaleNumeric()}}
\item \href{#method-scaleData}{\code{pgu.model$scaleData()}}
\item \href{#method-rescaleNumeric}{\code{pgu.model$rescaleNumeric()}}
\item \href{#method-rescaleData}{\code{pgu.model$rescaleData()}}
\item \href{#method-modelParameterData}{\code{pgu.model$modelParameterData()}}
\item \href{#method-modelParameterFeature}{\code{pgu.model$modelParameterFeature()}}
\item \href{#method-modelQualityData}{\code{pgu.model$modelQualityData()}}
\item \href{#method-modelQualityFeature}{\code{pgu.model$modelQualityFeature()}}
\item \href{#method-fitResultData}{\code{pgu.model$fitResultData()}}
\item \href{#method-fitResultFeature}{\code{pgu.model$fitResultFeature()}}
\item \href{#method-testResultData}{\code{pgu.model$testResultData()}}
\item \href{#method-testResultFeature}{\code{pgu.model$testResultFeature()}}
\item \href{#method-plotModel}{\code{pgu.model$plotModel()}}
\item \href{#method-clone}{\code{pgu.model$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates and returns a new \code{pgu.model} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$new(data = "tbl_df")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{The data to be analyzed.
(tibble::tibble)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new \code{pgu.model} object.
(pguIMP::pgu.model)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-finalize"></a>}}
\if{latex}{\out{\hypertarget{method-finalize}{}}}
\subsection{Method \code{finalize()}}{
Clears the heap and
indicates that instance of \code{pgu.model} is removed from heap.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$finalize()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Prints instance variables of a \code{pgu.model} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
string
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-resetModelParameter"></a>}}
\if{latex}{\out{\hypertarget{method-resetModelParameter}{}}}
\subsection{Method \code{resetModelParameter()}}{
Resets instance variable \code{modelParameter}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$resetModelParameter(data = "tbl_df")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Dataframe to be analyzed.
(tibble::tibble)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-resetModelList"></a>}}
\if{latex}{\out{\hypertarget{method-resetModelList}{}}}
\subsection{Method \code{resetModelList()}}{
Resets instance variable \code{modelList}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$resetModelList(data = "tbl_df")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Dataframe to be analyzed.
(tibble::tibble)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-resetModel"></a>}}
\if{latex}{\out{\hypertarget{method-resetModel}{}}}
\subsection{Method \code{resetModel()}}{
Resets instance variable \code{modelList}.
Resets instance variable \code{modelParameter}.
Displays progress if shiny is loaded.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$resetModel(data = "tbl_df", progress = "Progress")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Dataframe to be analyzed.
(tibble::tibble)}

\item{\code{progress}}{If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
(shiny::Progress)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-setNormDist"></a>}}
\if{latex}{\out{\hypertarget{method-setNormDist}{}}}
\subsection{Method \code{setNormDist()}}{
Stores the information of a pgu.norDist object in an
entry of the instance variable \code{modelList}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$setNormDist(data = "pgu.normDist", feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Instance of pgu.normDist
(pguIMP::pgu.normDist)}

\item{\code{feature}}{Attribute corresponding to the pgu.normDist object data.
(character)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-featureIdx"></a>}}
\if{latex}{\out{\hypertarget{method-featureIdx}{}}}
\subsection{Method \code{featureIdx()}}{
Returns the index of a pgu.normDist object wihtin the instance variable \code{modelParameter}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$featureIdx(feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Index of attribute entry in dataframe
(numeric)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fitFeature"></a>}}
\if{latex}{\out{\hypertarget{method-fitFeature}{}}}
\subsection{Method \code{fitFeature()}}{
Runs the fit function of a pgu.normDist object at a
user denied position within the instance variable modelList.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$fitFeature(feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fitData"></a>}}
\if{latex}{\out{\hypertarget{method-fitData}{}}}
\subsection{Method \code{fitData()}}{
Loops through all attributes and calls the object's
ftiFeature function.
Displays progress if shiny is loaded.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$fitData(progress = "Progress")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{progress}}{If shiny is loaded, the analysis' progress is stored in this instance of the shiny Progress class.
(shiny::Progress)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-logFitResultsFeature"></a>}}
\if{latex}{\out{\hypertarget{method-logFitResultsFeature}{}}}
\subsection{Method \code{logFitResultsFeature()}}{
Stores results from fitting procedure of a user defined attribute
into the corrsponding attribute of instance variable \code{modelParameter}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$logFitResultsFeature(feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-logFailedFitResultsFeature"></a>}}
\if{latex}{\out{\hypertarget{method-logFailedFitResultsFeature}{}}}
\subsection{Method \code{logFailedFitResultsFeature()}}{
Stores results from fitting procedure of a user defined attribute
into the corrsponding attribute of instance variable \code{modelParameter}
in case of a failed fitting routine.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$logFailedFitResultsFeature(feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-scaleNumeric"></a>}}
\if{latex}{\out{\hypertarget{method-scaleNumeric}{}}}
\subsection{Method \code{scaleNumeric()}}{
Scales numeric data based upon the model of a user defined attribute.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$scaleNumeric(value = "numeric", feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{Numeric vector
(numeric)}

\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
scaled version of the given vector
(numeric)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-scaleData"></a>}}
\if{latex}{\out{\hypertarget{method-scaleData}{}}}
\subsection{Method \code{scaleData()}}{
Scales a dataframe based upon  a list of models
stored in the instance variable modelList..
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$scaleData(data = "tbl_df")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Dataframe to be analyzed.
(tibble::tibble)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
scaled version of the given dataframe
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-rescaleNumeric"></a>}}
\if{latex}{\out{\hypertarget{method-rescaleNumeric}{}}}
\subsection{Method \code{rescaleNumeric()}}{
Re-scales numeric data based upon the model of a user defined attribute.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$rescaleNumeric(value = "numeric", feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{Numeric vector
(numeric)}

\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Re-scaled version of the given vector
(numeric)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-rescaleData"></a>}}
\if{latex}{\out{\hypertarget{method-rescaleData}{}}}
\subsection{Method \code{rescaleData()}}{
Re-scales a dataframe based upon  a list of models
stored in the instance variable modelList..
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$rescaleData(data = "tbl_df")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Dataframe to be analyzed.
(tibble::tibble)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Re-scaled version of the given dataframe
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-modelParameterData"></a>}}
\if{latex}{\out{\hypertarget{method-modelParameterData}{}}}
\subsection{Method \code{modelParameterData()}}{
Returns the model parameter (expectation value, standard deviation).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$modelParameterData()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Dataframe comprising model parameter.
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-modelParameterFeature"></a>}}
\if{latex}{\out{\hypertarget{method-modelParameterFeature}{}}}
\subsection{Method \code{modelParameterFeature()}}{
Returns the model parameter (expectation value, standard deviation)
for a user deined attribute.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$modelParameterFeature(feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Dataframe comprising model parameter.
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-modelQualityData"></a>}}
\if{latex}{\out{\hypertarget{method-modelQualityData}{}}}
\subsection{Method \code{modelQualityData()}}{
Returns the model parameters connected to model quality.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$modelQualityData()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Dataframe comprising model parameter.
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-modelQualityFeature"></a>}}
\if{latex}{\out{\hypertarget{method-modelQualityFeature}{}}}
\subsection{Method \code{modelQualityFeature()}}{
Returns the model parameters connected to model quality
for a user deined attribute.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$modelQualityFeature(feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Dataframe comprising model parameter.
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fitResultData"></a>}}
\if{latex}{\out{\hypertarget{method-fitResultData}{}}}
\subsection{Method \code{fitResultData()}}{
Returns the model fit results.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$fitResultData()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Dataframe comprising model fit results.
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-fitResultFeature"></a>}}
\if{latex}{\out{\hypertarget{method-fitResultFeature}{}}}
\subsection{Method \code{fitResultFeature()}}{
Returns the model fit results
for a user deined attribute.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$fitResultFeature(feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Dataframe comprising model fit results.
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-testResultData"></a>}}
\if{latex}{\out{\hypertarget{method-testResultData}{}}}
\subsection{Method \code{testResultData()}}{
Returns the hypothesis test results.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$testResultData()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
Dataframe comprising the hypothesis test results.
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-testResultFeature"></a>}}
\if{latex}{\out{\hypertarget{method-testResultFeature}{}}}
\subsection{Method \code{testResultFeature()}}{
Returns the hypothesis test results.
for a user deined attribute.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$testResultFeature(feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Dataframe comprising the hypothesis test results.
(tibble::tibble)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plotModel"></a>}}
\if{latex}{\out{\hypertarget{method-plotModel}{}}}
\subsection{Method \code{plotModel()}}{
Creates and returns a composite graphical analysis
of the modeling procedure of
a user defined attribute.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$plotModel(feature = "character")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature}}{Attribute's name.
(character)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Composite result plot.
(ggplot2::ggplot)
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{pgu.model$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
