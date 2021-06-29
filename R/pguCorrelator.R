#' @title pgu.correlator
#'
#' @description
#' An R6 class that performs pairwise correlation on the pguIMP data set.
#'
#' @format [R6::R6Class] object.
#' @section Construction:
#' x <- pguIMP::pgu.correlator$new()
#'
#' @importFrom R6 R6Class
#' @importFrom magrittr %>%
#' @importFrom dplyr select select_if pull rename mutate everything
#' @importFrom tibble tibble as_tibble is_tibble rownames_to_column
#'
#' @importFrom stats cor.test
#' @examples
#' require(dplyr)
#' require(tibble)
#' data(iris)
#' data_df <- iris %>%
#'   tibble::as_tibble() %>%
#'   dplyr::select(-c("Species"))
#' correlator = pguIMP::pgu.correlator$new(data_df)
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#' @export
#'

pgu.correlator <- R6::R6Class("pgu.correlator",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .featureNames = "character",
                                 .method = "character",
                                 .r = "matrix",
                                 .pPearson = "matrix",
                                 .tau = "matrix",
                                 .pKendall = "matrix",
                                 .rho = "matrix",
                                 .pSpearman = "matrix",
                                 .abscissa = "character",
                                 .ordinate = "character",
                                 .test = "htest"
                               ),
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
                                 #' @field setFeatureNames
                                 #' Sets the instance variable featureNames.
                                 #' It further initializes the instance variables:
                                 #' intercept, pIntercept, slope, pSlope.
                                 #' (character)
                                 setFeatureNames = function(names = "character"){
                                   private$.featureNames <- names
                                   private$.r <- self$resetMatrix(value = 0)
                                   private$.pPearson <- self$resetMatrix(value = 1)
                                   private$.tau <- self$resetMatrix(value = 0)
                                   private$.pKendall <- self$resetMatrix(value = 1)
                                   private$.rho <- self$resetMatrix(value = 0)
                                   private$.pSpearman <- self$resetMatrix(value = 1)
                                 },
                                 #' @field method
                                 #' Returns the instance variable method.
                                 #' (character)
                                 method = function(){
                                   return(private$.method)
                                 },
                                 #' @field r
                                 #' Returns the instance variable r.
                                 #' (matrix)
                                 r = function(){
                                   return(private$.r)
                                 },
                                 #' @field pPearson
                                 #' Returns the instance variable pPearson.
                                 #' (matrix)
                                 pPearson = function(){
                                   return(private$.pPearson)
                                 },
                                 #' @field tau
                                 #' Returns the instance variable tau.
                                 #' (matrix)
                                 tau = function(){
                                   return(private$.tau)
                                 },
                                 #' @field pKendall
                                 #' Returns the instance variable pKendall.
                                 #' (matrix)
                                 pKendall = function(){
                                   return(private$.pKendall)
                                 },
                                 #' @field rho
                                 #' Returns the instance variable rho.
                                 #' (matrix)
                                 rho = function(){
                                   return(private$.rho)
                                 },
                                 #' @field pSpearman
                                 #' Returns the instance variable pSpearman.
                                 #' (matrix)
                                 pSpearman = function(){
                                   return(private$.pSpearman)
                                 },
                                 #' @field abscissa
                                 #' Returns the instance variable abscissa.
                                 #' (character)
                                 abscissa = function(){
                                   return(private$.abscissa)
                                 },
                                 #' @field setAbscissa
                                 #' Sets the instance variable abscicca to value.
                                 setAbscissa = function(value = "character"){
                                   private$.abscissa <- value
                                 },
                                 #' @field ordinate
                                 #' Returns the instance variable ordinate.
                                 #' (character)
                                 ordinate = function(){
                                   return(private$.ordinate)
                                 },
                                 #' @field setOrdinate
                                 #' Sets the instance variable ordinate to value.
                                 setOrdinate = function(value = "character"){
                                   private$.ordinate <- value
                                 },
                                 #' @field test
                                 #' Returns the instance variable test.
                                 #' (stats::cor.test)
                                 test = function(){
                                   return(private$.test)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 #' @description
                                 #' Creates and returns a new `pgu.correlator` object.
                                 #' @param data
                                 #' The data to be modeled.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A new `pgu.correlator` object.
                                 #' (pguIMP::pgu.correlator)
                                 initialize = function(data = "tbl_df"){
                                   if(!tibble::is_tibble(data)){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   self$resetCorrelator(data)
                                 },
                                 #' @description
                                 #' Clears the heap and
                                 #' indicates if instance of `pgu.correlator` is removed from heap.
                                 finalize = function(){
                                   print("Instance of pgu.correlator removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 #' @description
                                 #' Prints instance variables of a `pgu.correlator` object.
                                 #' @return
                                 #' string
                                 print = function(){
                                   rString <- sprintf("\npgu.correlator\n")
                                   cat(rString)
                                   print(self$featureNames)
                                   mString <- sprintf("\nmethod: %s\n", self$method[1])
                                   cat(mString)
                                   coefString <- sprintf("\nPearson's r:\n")
                                   cat(coefString)
                                   print(self$r)
                                   pString <- sprintf("\nPearson's p-Value:\n")
                                   cat(pString)
                                   print(self$pPearson)
                                   mString <- sprintf("\nmethod: %s\n", self$method[2])
                                   cat(mString)
                                   coefString <- sprintf("\nKendall's tau:\n")
                                   cat(coefString)
                                   print(self$tau)
                                   pString <- sprintf("\nKendall's p-Value:\n")
                                   cat(pString)
                                   print(self$pKendall)
                                   mString <- sprintf("\nmethod: %s\n", self$method[3])
                                   cat(mString)
                                   coefString <- sprintf("\nSpearman's rho:\n")
                                   cat(coefString)
                                   print(self$rho)
                                   pString <- sprintf("\nSpearman's p-Value:\n")
                                   cat(pString)
                                   print(self$pPearson)
                                   cat("\n\n")
                                   invisible(self)
                                 },#function
                                 ####################
                                 # public functions #
                                 ####################
                                 #' @description
                                 #' Performes pair-wise correlation analysis on the attributes of the data frame.
                                 #' Progresse is indicated by the progress object passed to the function.
                                 #' @param data
                                 #' Dataframe with at least two numeric attributes.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' Keeps track of the analysis progress.
                                 #' (shiny::Progress)
                                 resetCorrelator = function(data = "tbl_df", progress = "Progress"){
                                   private$.featureNames <- data %>%
                                     dplyr::select_if(is.numeric) %>%
                                     colnames()
                                   private$.method <- c("pearson", "kendall","spearman")
                                   private$.r <- self$resetMatrix(value = 0)
                                   private$.pPearson <- self$resetMatrix(value = 1)
                                   private$.tau <- self$resetMatrix(value = 0)
                                   private$.pKendall <- self$resetMatrix(value = 1)
                                   private$.rho <- self$resetMatrix(value = 0)
                                   private$.pSpearman <- self$resetMatrix(value = 1)
                                   self$correlate(data, progress)
                                 },#function
                                 #' @description
                                 #' Creates a square matrix which dimension corresponds to the length
                                 #' of the instance variable featureNames. The matrix entries are set to a distict `value`.
                                 #' @param value
                                 #' The value the matrix entries are set to.
                                 #' (numeric)
                                 #' @return
                                 #' A square matrix.
                                 #' (matrix)
                                 resetMatrix = function(value = "numeric"){
                                   n = length(self$featureNames)
                                   df <- matrix(data = value,
                                                nrow = n,
                                                ncol = n,
                                                dimnames = list(self$featureNames, self$featureNames))
                                   return(df)
                                 },#function
                                 ####################
                                 # helper functions #
                                 ####################
                                 #' @description
                                 #' Determines the numerical index of the column of an attribute based on the attribute name.
                                 #' @param feature
                                 #' The attribute's name.
                                 #' (character)
                                 #' @return
                                 #' The attributes column index.
                                 #' (numeric)
                                 featureIdx = function(feature = "character"){
                                   idx <- match(feature, self$featureNames)
                                   if(is.na(idx)){
                                     rString <- sprintf("\nWarning in pgu.correlator: feature %s is not known\n",
                                                        feature)
                                     cat(rString)
                                   }
                                   return(idx)
                                 },#function
                                 #########################
                                 # correlation functions #
                                 #########################
                                 #' @description
                                 #' Creates a correlation test between two attributes of a dataframe.
                                 #' The test is stored as instance variable.
                                 #' @param abscissa
                                 #' The abscissa values.
                                 #' (numeric)
                                 #' @param ordinate
                                 #' The ordinate values.
                                 #' (numeric)
                                 #' @param method
                                 #' The cname of the correlation test.
                                 #' Valid coiced are defined by the instance variable `method`.
                                 #' (chatacter)
                                 calcCorrelationNumeric = function(abscissa = "numeric", ordinate = "numeric", method = "character"){
                                   private$.test <- stats::cor.test(x = abscissa,
                                                                    y = ordinate,
                                                                    alternative = "two.sided",
                                                                    exact = FALSE,
                                                                    method = method)
                                 },#function
                                 #' @description
                                 #' Performs the actual correlation test routine after Pearson.
                                 #' Iteratively runs through the attributes known to the class
                                 #' and calculates Pearson's correlation for each valid attribute pair.
                                 #' The test results are stored in the instance variables:
                                 #' r, pPearson.
                                 #' Here, pX represents the p-value of the respective parameter X.
                                 #' Displays the progress if shiny is loaded.
                                 #' @param data
                                 #' The data to be analysed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored within this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 createCorrelationMatrixPearson = function(data = "tbl_df", progress = "Progress"){
                                   for (abs in self$featureNames){
                                     for (ord in self$featureNames){
                                       if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                         progress$inc(1)
                                       }#if
                                       self$calcCorrelationNumeric(abscissa = data %>%
                                                                     dplyr::pull(abs),
                                                                   ordinate = data %>%
                                                                     dplyr::pull(ord),
                                                                   method = self$method[1])
                                       private$.r[ord,abs] <-self$test$estimate[[1]]
                                       private$.pPearson[ord, abs] <- self$test$p.value
                                     }#for
                                   }#for
                                 },#function
                                 #' @description
                                 #' Performs the actual correlation test routine after Kendall.
                                 #' Iteratively runs through the attributes known to the class
                                 #' and calculates Kendall's correlation for each valid attribute pair.
                                 #' The test results are stored in the instance variables:
                                 #' tau, pKendall.
                                 #' Here, pX represents the p-value of the respective parameter X.
                                 #' Displays the progress if shiny is loaded.
                                 #' @param data
                                 #' The data to be analysed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored within this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 createCorrelationMatrixKendall = function(data = "tbl_df", progress = "Progress"){
                                   for (abs in self$featureNames){
                                     for (ord in self$featureNames){
                                       if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                         progress$inc(1)
                                       }#if
                                       self$calcCorrelationNumeric(abscissa = data %>%
                                                                     dplyr::pull(abs),
                                                                   ordinate = data %>%
                                                                     dplyr::pull(ord),
                                                                   method = self$method[2])
                                       private$.tau[ord,abs] <-self$test$estimate[[1]]
                                       private$.pKendall[ord, abs] <- self$test$p.value
                                     }#for
                                   }#for
                                 },#function

                                 #' @description
                                 #' Performs the actual correlation test routine after Spearman.
                                 #' Iteratively runs through the attributes known to the class
                                 #' and calculates Spearman's correlation for each valid attribute pair.
                                 #' The test results are stored in the instance variables:
                                 #' rho, pSpearman.
                                 #' Here, pX represents the p-value of the respective parameter X.
                                 #' Displays the progress if shiny is loaded.
                                 #' @param data
                                 #' The data to be analysed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored within this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 createCorrelationMatrixSpearman = function(data = "tbl_df", progress = "Progress"){
                                   for (abs in self$featureNames){
                                     for (ord in self$featureNames){
                                       if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                         progress$inc(1)
                                       }#if
                                       self$calcCorrelationNumeric(abscissa = data %>%
                                                                     dplyr::pull(abs),
                                                                   ordinate = data %>%
                                                                     dplyr::pull(ord),
                                                                   method = self$method[3])
                                       private$.rho[ord,abs] <-self$test$estimate[[1]]
                                       private$.pSpearman[ord, abs] <- self$test$p.value
                                     }#for
                                   }#for
                                 },#function

                                 #' @description
                                 #' Performs the all three correlation test routines defined
                                 #' within the instance variable `method`.
                                 #' Displays the progress if shiny is loaded.
                                 #' @param data
                                 #' The data to be analysed.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored within this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 correlate = function(data = "tbl_df", progress = "Progress"){
                                   self$createCorrelationMatrixPearson(data, progress)
                                   self$createCorrelationMatrixKendall(data, progress)
                                   self$createCorrelationMatrixSpearman(data, progress)
                                 },#function

                                 ###################
                                 # print functions #
                                 ###################
                                 #' @description
                                 #' Transforms the results of the correlation procedure for a valid pair of attributes to a dataframe
                                 #' and returns it.
                                 #' @return
                                 #' The analyis result as a dataframe.
                                 #' (tibble::tibble)
                                 printFeature = function(){
                                   df <- data.frame(
                                     abscissa = self$abscissa,
                                     ordinate = self$ordinate,
                                     r = self$r[self$ordinate, self$abscissa],
                                     p.Pearson = self$pPearson[self$ordinate, self$abscissa],
                                     tau = self$tau[self$ordinate, self$abscissa],
                                     p.Kendall = self$pPearson[self$ordinate, self$abscissa],
                                     rho = self$rho[self$ordinate, self$abscissa],
                                     p.Spearman = self$pSpearman[self$ordinate, self$abscissa]) %>%
                                     t() %>%
                                     as.data.frame() %>%
                                     tibble::rownames_to_column("correlation parameter") %>%
                                     tibble::as_tibble() %>%
                                     dplyr::rename(value = "V1")
                                   return(df)
                                 },#function

                                 #' @description
                                 #' Transfroms instance variable `r` to a dataframe and returns it.
                                 #' @return
                                 #' Dataframe of instance variable `r`.
                                 #' (tibble::tibble)
                                 printRTbl = function(){
                                   self$r %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$featureNames) %>%
                                     dplyr::select(features, dplyr::everything()) %>%
                                     return()
                                 },#function

                                 #' @description
                                 #' Transfroms instance variable `pPearson` to a dataframe and returns it.
                                 #' @return
                                 #' Dataframe of instance variable `pPearson`.
                                 #' (tibble::tibble)
                                 printPPearsonTbl = function(){
                                   self$pPearson %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$featureNames) %>%
                                     dplyr::select(features, dplyr::everything()) %>%
                                     return()
                                 },#function

                                 #' @description
                                 #' Transfroms instance variable `tau` to a dataframe and returns it.
                                 #' @return
                                 #' Dataframe of instance variable `tau`.
                                 #' (tibble::tibble)
                                 printTauTbl = function(){
                                   self$tau %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$featureNames) %>%
                                     dplyr::select(features, dplyr::everything()) %>%
                                     return()
                                 },#function

                                 #' @description
                                 #' Transfroms instance variable `pKendall` to a dataframe and returns it.
                                 #' @return
                                 #' Dataframe of instance variable `pKendall`.
                                 #' (tibble::tibble)
                                 printPKendallTbl = function(){
                                   self$pKendall %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$featureNames) %>%
                                     dplyr::select(features, dplyr::everything()) %>%
                                     return()
                                 },#function

                                 #' @description
                                 #' Transfroms instance variable `rho` to a dataframe and returns it.
                                 #' @return
                                 #' Dataframe of instance variable `rho`.
                                 #' (tibble::tibble)
                                 printRhoTbl = function(){
                                   self$rho %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$featureNames) %>%
                                     dplyr::select(features, dplyr::everything()) %>%
                                     return()
                                 },#function

                                 #' @description
                                 #' Transfroms instance variable `pSpearman` to a dataframe and returns it.
                                 #' @return
                                 #' Dataframe of instance variable `pSpearman`.
                                 #' (tibble::tibble)
                                 printPSpearmanTbl = function(){
                                   self$pSpearman %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$featureNames) %>%
                                     dplyr::select(features, dplyr::everything()) %>%
                                     return()
                                 }#function

                               )#public
)#Class
