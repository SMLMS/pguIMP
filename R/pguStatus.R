#' @title pgu.status
#'
#' @description
#' A class that keeps track of the pguIMP analysis process.
#'
#' @details
#' pguIMP uses a defined linear analysis path
#' The current status therefore provides information about
#' which analyses have already been performed and which
#' still have to be performed.
#' This way pguIMG knows any time during analysis,
#' if all necessary information for the next desired analysis
#' step are available.
#' This object is used by the shiny based gui and is not for use in individual R-scripts!
#'
#' @format [R6::R6Class] object.
#'
#' @importFrom R6 R6Class
#'
#' @author Sebastian Malkusch, \email{malkusch@@med.uni-frankfurt.de}
#'
#' @export
#'


pgu.status <- R6::R6Class("pgu.status",
                        ####################
                        # instance variables
                        ####################
                        private = list(
                          .processAlphabet = "character",
                          .processStatus = "logical"
                        ),
                        ##################
                        # accessor methods
                        ##################
                        active = list(
                          #' @field processAlphabet
                          #' Reurns the process alphabet of
                          #' the pguIMP analysis routine.
                          #' (character)
                          processAlphabet = function(){
                            return(private$.processAlphabet)
                          },
                          #' @field processStatus
                          #' Returns the process status of the pguIMP routine.
                          #' (logical)
                          processStatus = function(){
                            return(private$.processStatus)
                          }
                        ),
                        ###################
                        # memory management
                        ###################
                        public = list(
                          #' @description
                          #' Creates and returns a new `pgu.status`` object.
                          #' @return
                          #' A new `pgu.status` object.
                          #' (pguIMP::pgu.status)
                          initialize = function() {
                            print("Instance of pgu.status allocated")
                            self$reset()
                          },
                          #' @description
                          #' Clears the heap and
                          #' indicates if instance of `pgu.status` is removed from heap.
                          finalize = function() {
                            print("Instance of pgu.status removed from heap")
                          },
                          ##########################
                          # print instance variables
                          ##########################
                          #' @description
                          #' Prints instance variables of a `pgu.status` object.
                          #' @return
                          #' string
                          print = function() {
                            message <- sprintf("\npgu.status\n\n")
                            for (i in (1:length(self$processAlphabet))){
                              message <- sprintf("%s%s:\t%s\n", message, self$processAlphabet[i], self$processStatus[i])
                            }
                            cat(message)

                            invisible(self)
                          },
                          ####################
                          # public functions #
                          ####################
                          #' @description
                          #' resets the intance variables `processAlphabet` and `processStatus` to their initial values (FALSE).
                          reset = function(){
                            private$.processAlphabet <- c("dataUploaded", "dataImported", "dataFiltered", "loqImported",
                                                          "loqDetected", "loqMutated", "modelOptimized", "modelDefined", "normalized", "naDetected",
                                                          "outliersDetected", "imputed", "validated")
                            private$.processStatus <- c(rep(FALSE, length(self$processAlphabet)))
                          },
                          #' @description
                          #' updates the process status
                          #' @param processName
                          #' The name of the `pguIMP` process that has been performed.
                          #' (character)
                          #' @param value
                          #' Indicates if the process ended with success.
                          #' (logical)
                          update = function(processName = "character", value = "logical"){
                            processFactor <- factor(processName, levels = self$processAlphabet)
                            idx <- as.integer(processFactor)
                            if(value){
                              private$.processStatus[1:idx] <- TRUE
                            }
                            private$.processStatus[idx:length(self$processAlphabet)] <- FALSE
                            private$.processStatus[idx] <- value
                          },
                          #' @description
                          #' Queries if a process has already been performed successfully.
                          #' @param processName
                          #' Name of the process to be checked.
                          #' (character)
                          #' @return
                          #' Status of the queried process
                          #' (logical)
                          query = function(processName = "character"){
                            processFactor <- factor(processName, levels = self$processAlphabet)
                            idx <- as.integer(processFactor)
                            return(self$processStatus[idx])
                          }
                        ) #public
)#R6Class
