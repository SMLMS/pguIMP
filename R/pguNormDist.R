#' @title pgu.normDist
#'
#' @description
#' Compares the distribution of a single attribute's values to normal distribution
#' by using several statistic tests.
#'
#' @details
#' The distribution of a single value is tested for normality by
#' Shapiro-Wilk test, Kolmogorov-Smirnov test, Anderson-Darling test.
#' The expectation value and standard deviation of a normal distribution
#' representing the data are determined by maximizing the log Likelihood
#' with respect to the expectation value and standard deviation.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom bbmle AIC AICc logLik mle2 residuals
#' @importFrom dplyr mutate
#' @importFrom ggplot2 aes aes_string element_rect geom_col geom_histogram geom_line geom_point
#' @importFrom ggplot2 geom_smooth ggplot ggplot_build ggtitle stat_qq stat_qq_line
#' @importFrom ggplot2 theme theme_linedraw xlab ylab
#' @importFrom magrittr %>%
#' @importFrom nortest ad.test lillie.test
#' @importFrom purrr discard
#' @importFrom R6 R6Class
#' @importFrom stats BIC ks.test na.omit optim shapiro.test
#' @importFrom tibble tibble
#'
#' @include dLogLikelihood.R
#' @include sLogLikelihood.R
#' @include normalDistribution.R
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'

pgu.normDist <- R6::R6Class("pgu.normDist",
                              ####################
                              # instance variables
                              ####################
                               private = list(
                                 .featureName = "character",
                                 .rawData = "tbl_df",
                                 .histogram = "tbl_df",
                                 .expMu = "numeric",
                                 .expSigma = "numeric",
                                 .dataPoints = "numeric",
                                 .logLikelihood = "numeric",
                                 .degOfFreedom = "numeric",
                                 .n = "integer",
                                 .bic = "numeric",
                                 .aic = "numeric",
                                 .aicc = "numeric",
                                 .rmse = "numeric",
                                 # Test results
                                 .fitSuccess = "logical",
                                 .testNames = "character",
                                 .testParameterNames = "character",
                                 .alpha = "numeric",
                                 # Shapiro Wilk test
                                 .w.shapiro = "numeric",
                                 .p.shapiro = "numeric",
                                 # Kolmogorow Smirnow test
                                 .d.kolmogorow = "numeric",
                                 .p.kolmogorow = "numeric",
                                 # Anderson Darling test
                                 .a.anderson = "numeric",
                                 .p.anderson = "numeric"
                               ),
                              ##################
                              # accessor methods
                              ##################
                               active = list(
                                 #' @field featureName
                                 #' Returns the instance variable featureName.
                                 #' (character)
                                 featureName = function(){
                                   return(private$.featureName)
                                 },
                                 #' @field rawData
                                 #' Returns the instance variable rawData.
                                 #' (tibble::tibble)
                                 rawData = function(){
                                   return(private$.rawData)
                                 },
                                 #' @field setRawData
                                 #' Sets the instance variable rawData.
                                 #' (tibble::tibble)
                                 setRawData = function(data = "tbl_df"){
                                   self$resetNormDist(data)
                                 },
                                 #' @field histogram
                                 #' Returns the instance variable histogram.
                                 #' (tibble::tibble)
                                 histogram = function(){
                                   return(private$.histogram)
                                 },
                                 #' @field expMu
                                 #' Returns the instance variable expMu.
                                 #' (numeric)
                                 expMu = function(){
                                   return(private$.expMu)
                                 },
                                 #' @field expSigma
                                 #' Returns the instance variable expSigma.
                                 #' (numeric)
                                 expSigma = function(){
                                   return(private$.expSigma)
                                 },
                                 #' @field dataPoints
                                 #' Returns the instance variable dataPoints.
                                 #' (numeric)
                                 dataPoints = function(){
                                   return(private$.dataPoints)
                                 },
                                 #' @field logLikelihood
                                 #' Returns the instance variable logLikelihood.
                                 #' (numeric)
                                 logLikelihood = function(){
                                   return(private$.logLikelihood)
                                 },
                                 #' @field degOfFreedom
                                 #' Returns the instance variable degOfFreedom.
                                 #' (numeric)
                                 degOfFreedom = function(){
                                   return(private$.degOfFreedom)
                                 },
                                 #' @field n
                                 #' Returns the instance variable n.
                                 #' (integer)
                                 n = function(){
                                   return(private$.n)
                                 },
                                 #' @field bic
                                 #' Returns the instance variable bic.
                                 #' (numeric)
                                 bic = function(){
                                   return(private$.bic)
                                 },
                                 #' @field aic
                                 #' Returns the instance variable aic.
                                 #' (numeric)
                                 aic = function(){
                                   return(private$.aic)
                                 },
                                 #' @field aicc
                                 #' Returns the instance variable aicc.
                                 #' (numeric)
                                 aicc = function(){
                                   return(private$.aicc)
                                 },
                                 #' @field rmse
                                 #' Returns the instance variable rmse.
                                 #' (numeric)
                                 rmse = function(){
                                   return(private$.rmse)
                                 },
                                 #' @field fitSuccess
                                 #' Returns the instance variable fitSuccess.
                                 #' (logical)
                                 fitSuccess = function(){
                                   return(private$.fitSuccess)
                                 },
                                 #' @field testNames
                                 #' Returns the instance variable testNames.
                                 #' (character)
                                 testNames = function(){
                                   return(private$.testNames)
                                 },
                                 #' @field testParameterNames
                                 #' Returns the instance variable testParameterNames.
                                 #' (character)
                                 testParameterNames = function(){
                                   return(private$.testParameterNames)
                                 },
                                 #' @field alpha
                                 #' Returns the instance variable alpha.
                                 #' (numeric)
                                 alpha = function(){
                                   return(private$.alpha)
                                 },
                                 #' @field w.shapiro
                                 #' Returns the instance variable w.shapiro.
                                 #' (numeric)
                                 w.shapiro = function(){
                                   return(private$.w.shapiro)
                                 },
                                 #' @field p.shapiro
                                 #' Returns the instance variable p.shapiro.
                                 #' (numeric)
                                 p.shapiro = function(){
                                   return(private$.p.shapiro)
                                 },
                                 #' @field d.kolmogorow
                                 #' Returns the instance variable d.kolmogorow.
                                 #' (numeric)
                                 d.kolmogorow = function(){
                                   return(private$.d.kolmogorow)
                                 },
                                 #' @field p.kolmogorow
                                 #' Returns the instance variable p.kolmogorow.
                                 #' (numeric)
                                 p.kolmogorow = function(){
                                   return(private$.p.kolmogorow)
                                 },
                                 #' @field a.anderson
                                 #' Returns the instance variable a.anderson.
                                 #' (numeric)
                                 a.anderson = function(){
                                   return(private$.a.anderson)
                                 },
                                 #' @field p.anderson
                                 #' Returns the instance variable p.anderson.
                                 #' (numeric)
                                 p.anderson = function(){
                                   return(private$.p.anderson)
                                 }
                                 ),
                              ###################
                              # memory management
                              ###################
                               public = list(
                                 #' @description
                                 #' Creates and returns a new `pgu.normDist` object.
                                 #' @param data
                                 #' The data to be analyzed.
                                 #' (tibble::tibble)
                                 #' @return
                                 #' A new `pgu.normDist` object.
                                 #' (pguIMP::pgu.normDist)
                                 initialize = function(data = "tbl_df"){
                                   if(class(data)[1] != "tbl_df"){
                                     data <- tibble::tibble(names <- "none",
                                                            values <- c(NA))
                                   }
                                   self$resetNormDist(data)
                                 }, #function

                                 #' @description
                                 #' Clears the heap and
                                 #' indicates that instance of `pgu.normDist` is removed from heap.
                                 finalize = function(){
                                   print("Instance of pgu.normDist removed from heap")
                                 },
                                 ##########################
                                 # print instance variables
                                 ##########################
                                 #' @description
                                 #' Prints instance variables of a `pgu.normDist` object.
                                 #' @return
                                 #' string
                                 print = function(){
                                   rString <- sprintf("\npgu.normDist:\n%s\n", self$featureName)
                                   cat(rString)
                                   cat(self$fitResult())
                                   cat(self$testResultCompendium())
                                   cat("\n\n")
                                   invisible(self)
                                 }, #function
                                 ####################
                                 # public functions #
                                 ####################
                                 #' @description
                                 #' Resets instance variables
                                 #' @param data
                                 #' Dataframe to be analyzed.
                                 #' (tibble::tibble)
                                 resetNormDist = function(data = "tbl_df"){
                                   if ((ncol(data) ==1) & (is.numeric(data[[1]]))){
                                     private$.featureName <- colnames(data)
                                   }#if
                                   else{
                                     rString <- sprintf("\nWarning in pgu.normDist: rawData is not numeric\n")
                                     cat(rString)
                                   }#else
                                   colnames(data) <- c("x")
                                   private$.rawData  <- stats::na.omit(data)
                                   private$.testNames <- c("Shapiro-Wilk", "Kolmogorow-Smirnow", "Anderson-Darling")
                                   private$.testParameterNames <- c("W", "D", "A")
                                   names(private$.testParameterNames) <- self$testNames
                                   private$.alpha <- 0.05
                                   private$.dataPoints <- length(self$rawData$x)
                                   private$.expMu <- 0
                                   private$.expSigma <- 0
                                   private$.logLikelihood <- 0
                                   private$.degOfFreedom <- 0
                                   private$.n <- 0
                                   private$.bic <- 0
                                   private$.aic <-0
                                   private$.aicc <- 0
                                   private$.rmse <- 0
                                   private$.fitSuccess <- FALSE
                                   private$.w.shapiro <- 0
                                   private$.p.shapiro <- 0
                                   private$.d.kolmogorow <- 0
                                   private$.p.kolmogorow <- 0
                                   private$.a.anderson <- 0
                                   private$.p.anderson <- 0
                                 }, #function

                                 #' @description
                                 #' Resets instance variables in case of a failed analysis.
                                 resetFail = function(){
                                   private$.expMu <- NA
                                   private$.expSigma <- NA
                                   private$.logLikelihood <- NA
                                   private$.degOfFreedom <- NA
                                   private$.n <- NA
                                   private$.bic <- NA
                                   private$.aic <-NA
                                   private$.aicc <- NA
                                   private$.rmse <- NA
                                   private$.fitSuccess <- FALSE
                                   private$.w.shapiro <- NA
                                   private$.p.shapiro <- NA
                                   private$.d.kolmogorow <- NA
                                   private$.p.kolmogorow <- NA
                                   private$.a.anderson <- NA
                                   private$.p.anderson <- NA
                                   private$.fitSuccess <- FALSE
                                 }, #function

                                 #####################
                                 # fitting functions #
                                 #####################
                                 #' @description
                                 #' Optimizes the logLikelihood between the data and a normal distribution
                                 #' with respect to the expectation value and standard deviation.
                                 #' The quality of the best model ist calculated subsequently.
                                 optimize = function(){
                                   estMu <- mean(self$rawData[["x"]], na.rm=TRUE)
                                   estSigma <- sd(self$rawData[["x"]], na.rm=TRUE)
                                   fit <- NULL
                                   tryCatch({
                                     fit <- stats::optim(par = c(mu = estMu, sigma = estSigma), fn = pguIMP::dLogLikelihood, x =  purrr::discard(self$rawData[["x"]], is.na))
                                     private$.fitSuccess <- TRUE
                                   },
                                   error = function(e) {
                                     self$resetFail()
                                     errorMesage <- sprintf("\nError in pgu.normDist during maximum likelihood estimation:\n%s", e)
                                     cat(errorMesage)
                                     private$.fitSuccess <- FALSE
                                     return(NA)
                                   })#tryCatch
                                   if(self$fitSuccess == TRUE){
                                     private$.expMu <- fit$par[1]
                                     private$.expSigma <- fit$par[2]
                                     private$.logLikelihood <- -1 * dLogLikelihood(x=purrr::discard(self$rawData[["x"]], is.na), pars = c(mu =fit$par[1], sigma = fit$par[2]))
                                     private$.degOfFreedom <- 2
                                     private$.n <- length(purrr::discard(self$rawData[["x"]], is.na))
                                     private$.bic <- self$degOfFreedom*log(self$n) - 2*self$logLikelihood
                                     private$.aic <- 2*self$degOfFreedom - 2 *self$logLikelihood
                                     private$.aicc <- self$aic +((2*self$degOfFreedom^2) +(2*self$degOfFreedom))/(self$n-self$degOfFreedom-2)
                                   }#if
                                   # fit<-tryCatch({
                                   #   bbmle::mle2(x ~ dLogLikelihood(mu=mu, sigma=sigma), start = list(mu=estMu, sigma=estSigma), data=self$rawData)
                                   # },
                                   # error = function(e) {
                                   #   self$resetFail()
                                   #   errorMesage <- sprintf("\nError in pgu.normDist during maximum likelihood optimization:\n%s", e)
                                   #   cat(errorMesage)
                                   #   return(NA)
                                   # })#tryCatch
                                   # if(isS4(fit)){
                                   #   private$.rawData["residuals"] <- bbmle::residuals(fit)
                                   #   private$.expMu <- fit@coef[1]
                                   #   private$.expSigma <- fit@coef[2]
                                   #   ll <- bbmle::logLik(fit)
                                   #   private$.logLikelihood <- ll[1]
                                   #   private$.degOfFreedom <- attr(ll, "df")
                                   #   private$.bic <- stats::BIC(fit)
                                   #   private$.aic <- bbmle::AIC(fit)
                                   #   private$.aicc <- bbmle::AICc(fit)
                                   #   private$.fitSuccess <- TRUE
                                   # }#if
                                 }, #function

                                 #' @description
                                 #' Creates a histogram from the instance variable `rawData`.
                                 #' The histogram is stored in the instance variable `histogram`.
                                 createHistogram = function(){
                                   rawHist <- ggplot2::ggplot_build(ggplot2::ggplot(data = self$rawData, mapping = ggplot2::aes_string(x="x"))+
                                                             ggplot2::geom_histogram(ggplot2::aes(y=..density..), bins = 30)
                                   )
                                   d <- tibble::tibble(x = rawHist$data[[1]]$x,
                                                       y_data =rawHist$data[[1]]$y)
                                   d["y_fit"] <- normalDistribution(x=d["x"], mu=self$expMu, sigma=self$expSigma)
                                   d <- dplyr::mutate(d, res =y_data - y_fit)
                                   private$.rmse <- sqrt(mean(d[["res"]]^2))
                                   private$.histogram <- d
                                 }, #function

                                 #' @description
                                 #' Performes a qq-analysis of the instance variable `rawData`
                                 #' The qq-analysis is stored in the attributes `sample_quantile`
                                 #' and `theoretical_quantile` of the instance variable `rawData`.
                                 normalQQData = function(){
                                   p <- ggplot2::ggplot(data=self$rawData, mapping=ggplot2::aes_string(sample="x"))+
                                     ggplot2::stat_qq()+
                                     ggplot2::stat_qq_line(color="blue")
                                   d <- ggplot2::ggplot_build(p)
                                   private$.rawData["sample_quantile"] <- d$data[[1]]$sample
                                   private$.rawData["theoretical_quantile"] <- d$data[[1]]$theoretical
                                 }, #function

                                 ################################
                                 # test functions for normality #
                                 ################################
                                 #' @description
                                 #' Performes Shapiro-Wilk's test for normality on the
                                 #' instance variable `rawData`.
                                 #' The test result is stored in the instance variable
                                 #' `w.shapiro`.
                                 #' The p-value of the test is stored in the instance variable
                                 #' `p.shapiro`
                                 test.shapiro = function(){
                                   test <- stats::shapiro.test(self$rawData$x)
                                   private$.w.shapiro <- test$statistic
                                   private$.p.shapiro <- test$p.value
                                 }, #function

                                 #' @description
                                 #' Performes Kolmogorow-Smirnow's test for normality on the
                                 #' instance variable `rawData`.
                                 #' The test result is stored in the instance variable
                                 #' `d.kolmogorow`.
                                 #' The p-value of the test is stored in the instance variable
                                 #' `p.kolmogorow`
                                 test.kolmogorow = function(){
                                   # test <- stats::ks.test(self$rawData$x, "pnorm", mean=self$expMu, sd=self$expSigma)
                                   test <- nortest::lillie.test(self$rawData$x)
                                   private$.d.kolmogorow <- test$statistic
                                   private$.p.kolmogorow <- test$p.value
                                 }, #function

                                 #' @description
                                 #' Performes Anderson-Darling's test for normality on the
                                 #' instance variable `rawData`.
                                 #' The test result is stored in the instance variable
                                 #' `a.anderson`.
                                 #' The p-value of the test is stored in the instance variable
                                 #' `p.anderson`
                                 test.anderson = function(){
                                   test <- nortest::ad.test(self$rawData$x)
                                   private$.a.anderson <- test$statistic
                                   private$.p.anderson <- test$p.value
                                 }, #function

                                 ##################
                                 # export results #
                                 ##################
                                 #' @description
                                 #' Returns the result of the classes optimize function
                                 #' in form of a formated string.
                                 #' @return
                                 #' String of the results of the fitting routine
                                 #' (character)
                                 fitResult = function(){
                                   s <- sprintf("MLE Fit Result:\nmu = %.5f\nsigma = %.5f\nlogLikelihood = %.5f\nnumber of data points = %i\ndegrees of freedom = %i\nBIC = %.5f\nAIC = %.5f\nAICc = %.5f\nRMSE = %.5f",
                                                self$expMu, self$expSigma, self$logLikelihood, self$dataPoints, self$degOfFreedom, self$bic, self$aic, self$aicc, self$rmse)
                                   return(s)
                                 }, #function

                                 #' @description
                                 #' Returns the result of the classes test functions
                                 #' in form of a formated string.
                                 #' @param testName
                                 #' Defines the test which result shall be returned.
                                 #' Can be of type:`Shapiro-Wilk`, `Kolmogorow-Smirnow`
                                 #' or `Anderson-Darling`.
                                 #' (character)
                                 #' @return
                                 #' String of the results of the testing routine
                                 #' (character)
                                 testResult = function(testName = "Shapiro-Wilk"){
                                   switch (testName,
                                           "Shapiro-Wilk"={
                                             name <- self$testNames[1]
                                             parameter <- self$testParameterNames[1]
                                             s <- self$w.shapiro
                                             p <- self$p.shapiro
                                           }, #Shapiro-Wilk
                                           "Kolmogorow-Smirnow"={
                                             name <- self$testNames[2]
                                             parameter <- self$testParameterNames[2]
                                             s <- self$d.kolmogorow
                                             p <- self$p.kolmogorow
                                           }, #Kolmogorow-Smornow
                                           "Anderson-Darling"={
                                             name <- self$testNames[3]
                                             parameter <- self$testParameterNames[3]
                                             s <- self$a.anderson
                                             p <- self$p.anderson
                                           } #Anderson-Darling
                                   )#switch
                                   s <- sprintf("%s test for normality:\n%s = %.5f\np = %.5f", name, parameter, s, p)
                                   return(s)
                                 }, #function


                                 #' @description
                                 #' Returns the result of the classes test functions
                                 #' `Shapiro-Wilk`, `Kolmogorow-Smirnow` and
                                 #' `Anderson-Darling`
                                 #' in form of a formated string.
                                 #' (character)
                                 #' @return
                                 #' String of the results of the testing routine
                                 #' (character)
                                 testResultCompendium = function(){
                                   s <- "\nStatistical test results:"
                                   for (name in self$testNames) {
                                     s <- paste(s, self$testResult(name), sep="\n")
                                   }
                                   return(s)
                                 }, #function

                                 ##################
                                 # plot functions #
                                 ##################
                                 #' @description
                                 #' Displays the instance variable `histogram`
                                 #' in form of a bar plot and overlays the corresponding normal distribution.
                                 #' @return
                                 #' A bar plot.
                                 #' (ggplot2::ggplot)
                                 plotHistogram = function(){
                                   p <- ggplot2::ggplot(data=self$histogram, mapping = ggplot2::aes_string(x="x", y="y_data"))+
                                     ggplot2::geom_col()+
                                     ggplot2::geom_line(ggplot2::aes_string(x="x", y="y_fit"), color="blue") +
                                     ggplot2::ggtitle(sprintf("Distribution of %s", self$featureName)) +
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
                                 #' Displays the residuals between the instance variable
                                 #' `histogram` and the corresponding normal distribution.
                                 #' @return
                                 #' A scatter plot.
                                 #' (ggplot2::ggplot)
                                 plotResiduals = function(){
                                   p <- ggplot2::ggplot(data=self$histogram, mapping=ggplot2::aes_string(x="x", y="res"))+
                                     ggplot2::geom_point()+
                                     ggplot2::geom_smooth(method = 'loess') +
                                     ggplot2::ggtitle("Residuals") +
                                     ggplot2::ylab("residuals") +
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
                                 #' Displays the distribution of the residuals between the distribution of the instance variable
                                 #' `histogram` in form of a histogram.
                                 #' @return
                                 #' A bar plot.
                                 #' (ggplot2::ggplot)
                                 plotResidualDist = function(){
                                   p <- ggplot2::ggplot(data=self$histogram, mapping=ggplot2::aes_string(x="res"))+
                                     ggplot2::geom_histogram(bins = 30) +
                                     ggplot2::ggtitle("Residual distribution") +
                                     ggplot2::ylab("counts") +
                                     ggplot2::xlab("residual value") +
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
                                 #' Displays the distribution of the residuals between the distribution of the instance variable
                                 #' `rawData` in form of a histogram.
                                 #' @return
                                 #' A bar plot.
                                 #' (ggplot2::ggplot)
                                 plotRawResidualDist = function(){
                                   p <- ggplot2::ggplot(data=self$rawData, mapping=ggplot2::aes_string(x="residuals"))+
                                     ggplot2::geom_histogram(bins = 30) +
                                     ggplot2::ggtitle("Residual distribution") +
                                     ggplot2::ylab("counts") +
                                     ggplot2::xlab("residual value") +
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
                                 #' Displays the distribution of the instance variable
                                 #' `rawData` in form of a histogram.
                                 #' @return
                                 #' A bar plot.
                                 #' (ggplot2::ggplot)
                                 plotRawDataDist = function(){
                                   p <- ggplot2::ggplot(data=self$rawData, mapping=ggplot2::aes_string(x="x"))+
                                     ggplot2::geom_histogram(bins = 30) +
                                     ggplot2::ggtitle("Feature distribution") +
                                     ggplot2::theme_linedraw() +
                                     ggplot2::ylab("counts") +
                                     ggplot2::xlab("value") +
                                     ggplot2::theme(
                                       panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                       plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                       legend.background = ggplot2::element_rect(fill = "transparent"),
                                       legend.key = ggplot2::element_rect(fill = "transparent")
                                     )
                                   return(p)
                                 }, #function

                                 #' @description
                                 #' Displays a qqplot of the instance variable
                                 #' `rawData`.
                                 #' @return
                                 #' A qq-plot.
                                 #' (ggplot2::ggplot)
                                 normalQQPlot = function(){
                                   p <- ggplot2::ggplot(data=self$rawData, mapping=ggplot2::aes_string(sample="x"))+
                                     ggplot2::stat_qq()+
                                     ggplot2::stat_qq_line(color="blue") +
                                     ggplot2::ggtitle("qq-plot") +
                                     ggplot2::ylab("data") +
                                     ggplot2::xlab("model") +
                                     ggplot2::theme_linedraw() +
                                     ggplot2::theme(
                                       panel.background = ggplot2::element_rect(fill = "transparent"), # bg of the panel
                                       plot.background = ggplot2::element_rect(fill = "transparent", color = NA), # bg of the plot
                                       legend.background = ggplot2::element_rect(fill = "transparent"),
                                       legend.key = ggplot2::element_rect(fill = "transparent")
                                     )
                                   return(p)
                                 }, #function

                                 ######################
                                 # compound functions #
                                 ######################
                                 #' @description
                                 #' Runs the optimization process and performs all implemented quality controls.
                                 #' Additionally performs hypothesis tests for nromality.
                                 fit = function(){
                                   self$optimize()
                                   if(self$fitSuccess){
                                     self$createHistogram()
                                     self$normalQQData()
                                     self$test.shapiro()
                                     self$test.kolmogorow()
                                     self$test.anderson()
                                   }#if
                                 }#fit
                               )#public
)#class
