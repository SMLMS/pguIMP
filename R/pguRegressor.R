#' @title pgu.regressor
#'
#' @description
#' A class that performs pairwise robust regression on the pguIMP data set.
#'
#' @details
#' A class that performs pairwise robust regression on the pguIMP data set.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom dplyr everything mutate select select_if sym
#' @importFrom ggplot2 aes aes_string element_blank geom_boxplot geom_histogram geom_point ggplot
#' @importFrom ggplot2 ggtitle stat_smooth theme
#' @importFrom gridExtra grid.arrange
#' @importFrom magrittr %>%
#' @importFrom R6 R6Class
#' @importFrom robust lmRob
#' @importFrom shiny Progress
#' @importFrom stats as.formula lm
#' @importFrom tibble as_tibble enframe tibble
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.regressor <- R6::R6Class("pgu.regressor",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .featureNames = "character",
                                 .intercept = "matrix",
                                 .pIntercept = "matrix",
                                 .slope = "matrix",
                                 .pSlope = "matrix",
                                 .model = "lmRob"
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
                                   private$.intercept <- self$resetMatrix(value = 0)
                                   private$.pIntercept <- self$resetMatrix(value = 1)
                                   private$.slope <- self$resetMatrix(value = 0)
                                   private$.pSlope <- self$resetMatrix(value = 1)
                                 },
                                 #' @field intercept
                                 #' Returns the instance variable intercept.
                                 #' (matrix)
                                 intercept = function(){
                                   return(private$.intercept)
                                 },
                                 #' @field pIntercept
                                 #' Returns instance variable pIntercept.
                                 #' (matrix)
                                 pIntercept = function(){
                                   return(private$.pIntercept)
                                 },
                                 #' @field slope
                                 #' Returns the instance variable slope.
                                 #' (matrix)
                                 slope = function(){
                                   return(private$.slope)
                                 },
                                 #' @field pSlope
                                 #' Returns the instance variable pSlope.
                                 #' (matrix)
                                 pSlope = function(){
                                   return(private$.pSlope)
                                 },
                                 #' @field model
                                 #' Returns the instance variable model.
                                 #' (robust::lmRob)
                                 model = function(){
                                   return(private$.model)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 #' @description
                                 #' Creates and returns a new `pgu.regressor` object.
                                 #' @param data
                                 #' The data to be modeled.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A new `pgu.regressor` object.
                                 #' (pguIMP::pgu.regressor)
                                 initialize = function(data = "tbl_df"){
                                   if(class(data)[1] != "tbl_df"){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   self$resetRegressor(data)
                                 },
                                 #' @description
                                 #' Clears the heap and
                                 #' indicates if instance of `pgu.regressor` is removed from heap.
                                 finalize = function(){
                                   print("Instance of pgu.regressor removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 #' @description
                                 #' Prints instance variables of a `pgu.regressor` object.
                                 #' @return
                                 #' string
                                 print = function(){
                                   rString <- sprintf("\npgu.regressor\n")
                                   cat(rString)
                                   fString <- sprintf("\nfeatureNames:\n")
                                   cat(fString)
                                   print(self$featureNames)
                                   iString <- sprintf("\nintercept matrix:\n")
                                   cat(iString)
                                   print(self$intercept)
                                   sString <- sprintf("\nslope matrix:\n")
                                   cat(sString)
                                   print(self$slope)
                                   pString <- sprintf("\np-Value matrix:\n")
                                   cat(pString)
                                   print(self$pValue)
                                   cat("\n\n")
                                   invisible(self)
                                 },
                                 ####################
                                 # public functions #
                                 ####################
                                 #' @description
                                 #' Performes pair-wise robust linear regression on the attributes of the data tibble.
                                 #' Progresse is indicated by the progress object passed to the function.
                                 #' @param data
                                 #' Dataframe with at least two numeric attributes.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' Keeps track of the analysis progress.
                                 #' (shiny::Progress)
                                 resetRegressor = function(data = "tbl_df", progress = "Progress"){
                                   self$setFeatureNames <- data %>%
                                     dplyr::select_if(is.numeric) %>%
                                     colnames()
                                   self$createRegressionMatrix(data, progress)
                                 },
                                 #' @description
                                 #' Sets the diagonal of a square matrix to NA.
                                 #' @param data
                                 #' The matrix whose diagonal is to be reset.
                                 #' (matrix)
                                 #' @return
                                 #' A matrix with its diagonal reset to NA.
                                 #' (matrix)
                                 resetDiagonal = function(data = "matrix"){
                                   if(nrow(data) == ncol(data)){
                                     for (i in 1:nrow(data)){
                                       data[i,i] <- NA
                                     }
                                   }
                                   else{
                                     print("Warning: Regressor matrix needs to be square")
                                   }
                                   return(data)
                                 },
                                 #' @description
                                 #' Creates a square matrix which dimension corresponds to the length
                                 #' of the instance variable featureNames. The matrix entries are set to a distict `value`.
                                 #' The diagonal is set to NA.
                                 #' @param value
                                 #' The value the matrix entries are set to.
                                 #' (numeric)
                                 #' @return
                                 #' A square matrix.
                                 #' (matrix)
                                 resetMatrix = function(value = "numeric"){
                                   n <- length(self$featureNames)
                                   df <- matrix(data = value,
                                                nrow = n,
                                                ncol = n,
                                                dimnames = list(self$featureNames, self$featureNames))
                                   if(sum(dim(df)) > 0){
                                     self$resetDiagonal(df)
                                   }
                                   return(df)
                                 },
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
                                   idx <- match(c(feature), self$featureNames)
                                   if(is.na(idx)){
                                     rString <- sprintf("\nWarning in pgu.regressor: feature %s is not known\n",
                                                        feature)
                                     cat(rString)
                                   }
                                   return(idx)
                                 },
                                 #' @description
                                 #' Checks if the feature is known to the class.
                                 #' @param feature
                                 #' An attribute's name that is to be checked.
                                 #' (character)
                                 #' @return
                                 #' The test result.
                                 #' (logical)
                                 featureIsValid = function(feature = "character"){
                                   idx <- self$featureIdx(feature)
                                   if(is.na(idx)){
                                     return(FALSE)
                                   }
                                   else{
                                     return(TRUE)
                                   }
                                 },
                                 #' @description
                                 #' Checks a if a pair of attributes different and known to the class.
                                 #' @param abscissa
                                 #' An attribute's name that is to be checked.
                                 #' (character)
                                 #' @param ordinate
                                 #' An attribute's name that is to be checked.
                                 #' (character)
                                 #' @return
                                 #' The test result.
                                 #' (logical)
                                 featurePairIsValid = function(abscissa = "character", ordinate = "character"){
                                   val <- TRUE
                                   if(!self$featureIsValid(abscissa)){val <- FALSE}
                                   if(!self$featureIsValid(ordinate)){val <- FALSE}
                                   if(abscissa == ordinate){val <- FALSE}
                                   return(val)
                                 },
                                 ######################################
                                 # robust libear regression functions #
                                 ######################################
                                 #' @description
                                 #' Creates a robust model of linear regression between two attributes of a dataframe.
                                 #' The model is stored as instance variable.
                                 #' @param data
                                 #' The data to be modeled.
                                 #' (tibble::tibble)
                                 #' @param abscissa
                                 #' An attribute's name that equals a column name in the data.
                                 #' (character)
                                 #' @param ordinate
                                 #' An attribute's name that equals a column name in the data.
                                 #' (character)
                                 createModel = function(data = "tbl_df", abscissa = "character", ordinate = "character"){
                                   if(self$featurePairIsValid(abscissa, ordinate)){
                                     ord <- paste0("`", ordinate, "`")
                                     abs <- paste0("`", abscissa, "`")
                                     private$.model <- paste(ord, abs, sep = "~") %>%
                                       stats::as.formula() %>%
                                       # stats::lm(data, na.action = na.omit)
                                       robust::lmRob(data, na.action = na.omit)
                                   }
                                 },
                                 #' @description
                                 #' Performs the actual robust linear regression routine.
                                 #' Iteratively runs through the attributes known to the class
                                 #' and creates a robust linear regression model for each valid attribute pair.
                                 #' The model results are stored in the instance variables:
                                 #' intercept, pIntercept, slope, pSlope.
                                 #' Here, pX represents the p-value of the respective parameter X.
                                 #' Displays the progress if shiny is loaded.
                                 #' @param data
                                 #' The data to be modeled.
                                 #' (tibble::tibble)
                                 #' @param progress
                                 #' If shiny is loaded, the analysis' progress is stored within this instance of the shiny Progress class.
                                 #' (shiny::Progress)
                                 createRegressionMatrix = function(data = "tbl_df", progress = "Progress"){
                                   for (abscissa in self$featureNames){
                                     for (ordinate in self$featureNames){
                                       if(("shiny" %in% (.packages())) & (class(progress)[1] == "Progress")){
                                         progress$inc(1)
                                       }
                                       abs <- dplyr::sym(abscissa)
                                       ord <- dplyr::sym(ordinate)
                                       if(self$featurePairIsValid(abscissa, ordinate)){
                                         self$createModel(data, abscissa, ordinate)
                                         private$.intercept[[as.name(ord), as.name(abs)]] <- as.numeric(c(summary(self$model)$coefficients[1,1]))
                                         private$.pIntercept[[as.name(ord), as.name(abs)]] <-as.numeric(c(summary(self$model)$coefficients[1,4]))
                                         private$.slope[[as.name(ord), as.name(abs)]] <- as.numeric(c(summary(self$model)$coefficients[2,1]))
                                         private$.pSlope[[as.name(ord), as.name(abs)]] <-as.numeric(c(summary(self$model)$coefficients[2,4]))
                                       }#if
                                       else{
                                         private$.intercept[[as.name(ord), as.name(abs)]] <- NA
                                         private$.pIntercept[[as.name(ord), as.name(abs)]] <-NA
                                         private$.slope[[as.name(ord), as.name(abs)]] <- NA
                                         private$.pSlope[[as.name(ord), as.name(abs)]] <-NA
                                       }#else
                                     }#for
                                   }#for
                                 },#function
                                 ###################
                                 # print functions #
                                 ###################
                                 #' @description
                                 #' Transforms the results of the modeling procedure for a valid pair of attributes to a dataframe
                                 #' and returns it.
                                 #' @param abscissa
                                 #' The name of the attribute which is assigned to the abscissa.
                                 #' (character)
                                 #' @param ordinate
                                 #' The name of the attribute which is assigned to the ordinate.
                                 #' (character)
                                 #' @return
                                 #' The analyis result as a dataframe.
                                 #' (tibble::tibble)
                                 printModel = function(abscissa = "character", ordinate = "character"){
                                   t <- NULL
                                   if(self$featurePairIsValid(abscissa, ordinate)){
                                     abs <- dplyr::sym(abscissa)
                                     ord <- dplyr::sym(ordinate)
                                     para <- c("abscissa", "ordinate", "intercept", "p.intercept", "slope", "p.slope")
                                     val <- c(abscissa,
                                              ordinate,
                                              sprintf("%.3e", self$intercept[[as.name(ord), as.name(abs)]]),
                                              sprintf("%.3e", self$pIntercept[[as.name(ord), as.name(abs)]]),
                                              sprintf("%.3e", self$slope[[as.name(ord), as.name(abs)]]),
                                              sprintf("%.3e", self$pSlope[[as.name(ord), as.name(abs)]])
                                     )
                                     t <- tibble::tibble(parameter = para,
                                                         vlaue = val)
                                   }#if
                                   return(t)
                                 },#function
                                 #' @description
                                 #' Transfroms instance variable intercept to a dataframe and returns it.
                                 #' @return
                                 #' Dataframe of instance variable intercept.
                                 #' (tibble::tibble)
                                 printInterceptTbl = function(){
                                   self$intercept %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$featureNames) %>%
                                     dplyr::select(features, dplyr::everything()) %>%
                                     return()
                                 },#function
                                 #' @description
                                 #' Transfroms instance variable pIntercept to a dataframe and returns it.
                                 #' @return
                                 #' Dataframe of instance variable pIntercept.
                                 #' (tibble::tibble)
                                 printPInterceptTbl = function(){
                                   self$pIntercept %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$featureNames) %>%
                                     dplyr::select(features, dplyr::everything()) %>%
                                     return()
                                 },#function
                                 #' @description
                                 #' Transfroms instance variable slope to a dataframe and returns it.
                                 #' @return
                                 #' Dataframe of instance variable slope.
                                 #' (tibble::tibble)
                                 printSlopeTbl = function(){
                                   self$slope %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$featureNames) %>%
                                     dplyr::select(features, dplyr::everything()) %>%
                                     return()
                                 },#function
                                 #' @description
                                 #' Transfroms instance variable pSlope to a dataframe and returns it.
                                 #' @return
                                 #' Dataframe of instance variable pSlope.
                                 #' (tibble::tibble)
                                 printPSlopeTbl = function(){
                                   self$pSlope %>%
                                     tibble::as_tibble() %>%
                                     dplyr::mutate(features = self$featureNames) %>%
                                     dplyr::select(features, dplyr::everything()) %>%
                                     return()
                                 },#function

                                 ##################
                                 # plot functions #
                                 ##################
                                 #' @description
                                 #' Creates a scatter plot of the model
                                 #' stored within the instance variable of the class.
                                 #' @return
                                 #' A scatter plot.
                                 #' (ggplot2::ggplot)
                                 plotRegression = function(){
                                   abs <- dplyr::sym(names(self$model$model)[2])
                                   ord <- dplyr::sym(names(self$model$model)[1])
                                   p <- ggplot2::ggplot(data = self$model$model,
                                                        ggplot2::aes_string(x=as.name(abs),
                                                                   y=as.name(ord)),
                                                        na.rm = TRUE)+
                                     ggplot2::geom_point()+
                                     ggplot2::stat_smooth(method = "lm") +
                                     ggplot2::ggtitle("Robust Model\nLinear Regression")
                                   return(p)
                                 },#function

                                 #' @description
                                 #' Creates a histogram of the residual distribution of the model
                                 #' stored within the instance variable of the class.
                                 #' @return
                                 #' A histogram plot.
                                 #' (ggplot2::ggplot)
                                 plotResidualDist = function(){
                                   p <- tibble::enframe(self$model$residuals, name=c("index")) %>%
                                     ggplot2::ggplot(mapping=ggplot2::aes_string(x="value"), na.rm=TRUE)+
                                     ggplot2::geom_histogram() +
                                     ggplot2::ggtitle("Residuals\nBar Plot") +
                                     ggplot2::theme(axis.title.y = ggplot2::element_blank())
                                   return(p)
                                 },#function
                                 #' @description
                                 #' Creates a box plot of the residual distribution of the model
                                 #' stored within the instance variable of the class.
                                 #' @return
                                 #' A box plot.
                                 #' (ggplot2::ggplot)
                                 plotResidualBox = function(){
                                   p <- tibble::enframe(self$model$residuals, name=c("index")) %>%
                                     ggplot2::ggplot(mapping=ggplot2::aes_string(y="value"), na.rm=TRUE)+
                                     ggplot2::geom_boxplot()+
                                     ggplot2::ggtitle("Residuals\nBox Plot") +
                                     ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                                                    axis.text.x = ggplot2::element_blank(),
                                                    axis.ticks.x = ggplot2::element_blank(),
                                                    axis.title.y = ggplot2::element_blank())
                                   return(p)
                                 },#function
                                 #' @description
                                 #' Creates a model of robust linear regression.
                                 #' Executes all graphical exploration functions of the class
                                 #' and creates a composite graph based on their results.
                                 #' @param data
                                 #' The data to be modeled.
                                 #' (tibble::tibble)
                                 #' @param abscissa
                                 #' The name of the attribute which is assigned to the abscissa.
                                 #' (character)
                                 #' @param ordinate
                                 #' The name of the attribute which is assigned to the ordinate.
                                 #' (character)
                                 #' @return
                                 #' A composite graph.
                                 #' (gridExtra::grid.arrange)
                                 plotModel = function(data = "tbl_df", abscissa = "character", ordinate = "character"){
                                   p <- NULL
                                   if(self$featurePairIsValid(abscissa, ordinate)){
                                     self$createModel(data, abscissa, ordinate)
                                     p1 <- self$plotRegression()
                                     p2 <- self$plotResidualBox()
                                     p3 <- self$plotResidualDist()
                                     p <- gridExtra::grid.arrange(p1,p2,p3, layout_matrix = rbind(c(1,1,1,2),c(1,1,1,3)))
                                   }
                                   return(p)
                                 }#function
                               )#public
)#class
