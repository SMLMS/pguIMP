#' @title centralValue
#' @description
#' Returns the central value of a variable.
#' @details
#' Function that obtains a statistic of centrality of a
#' variable, given a sample of values.
#' If the variable is numeric it returns de median, if it
#' is a factor it returns the mode. In other cases it
#' tries to convert to a factor and then returns the mode.
#' Taken from:
#' https://github.com/ltorgo/DMwR2/
#' @author Luis Torgo
#' @param x
#' variable
#' @param ws
#' weights
#' @return
#' central value
#' @importFrom stats aggregate
#' @export
#' @examples
#' centralValue(x = seq(1,10,1))
#'
centralValue <- function(x, ws=NULL) {
  x <- unlist(x)  # because of dplyr structures not dropping (errors with dat[,i])
  if (is.numeric(x)) {
    if (is.null(ws)) median(x,na.rm=TRUE)
    else if ((s <- sum(ws)) > 0) sum(x*(ws/s)) else NA
  } else {
    x <- as.factor(x)
    if (is.null(ws)) levels(x)[which.max(table(x))]
    else levels(x)[which.max(stats::aggregate(ws,list(x),sum)[,2])]
  }
}


#' @title knnImputation
#' @description
#' Imputes missings using kNN.
#' @details
#' Function that fills in all unknowns using the k Nearest
#' Neighbours of each case with unknows.
#' By default it uses the values of the neighbours and
#' obtains an weighted (by the distance to the case) average
#' of their values to fill in the unknows.
#' If meth='median' it uses the median/most frequent value,
#' instead.
#' Taken from
#' https://github.com/ltorgo/DMwR2/
#' @author Luis Torgo
#' @param data
#' data frame containing missing values
#' @param k
#' number of nearest neighbors
#' @param scale
#' Indicates if data should be scaled
#' @param meth
#' Method for estimating the missing value
#' @param distData
#' Distance to the case
#' @return
#' cleaned data
#' @importFrom stats complete.cases
#' @export
#' @examples
#' centralValue(x = seq(1,10,1))
#'
knnImputation <- function(data,k=10,scale=TRUE,meth='weighAvg',distData=NULL) {

  n <- nrow(data)
  if (!is.null(distData)) {
    distInit <- n+1
    data <- rbind(data,distData)
  } else distInit <- 1
  N <- nrow(data)

  ncol <- ncol(data)
  ##nomAttrs <- rep(F,ncol)
  ##for(i in seq(ncol)) nomAttrs[i] <- is.factor(data[,i])
  ##nomAttrs <- which(nomAttrs)
  ##contAttrs <- setdiff(seq(ncol),nomAttrs)
  contAttrs <- which(vapply(data,dplyr::type_sum,character(1)) %in% c("dbl","int"))
  nomAttrs <- setdiff(seq.int(ncol),contAttrs)
  hasNom <- length(nomAttrs)

  dm <- data
  if (scale) dm[,contAttrs] <- scale(dm[,contAttrs])
  if (hasNom)
    for(i in nomAttrs) dm[[i]] <- as.integer(dm[[i]])

  dm <- as.matrix(dm)

  nas <- which(!stats::complete.cases(dm))
  if (!is.null(distData)) tgt.nas <- nas[nas <= n]
  else tgt.nas <- nas

  if (length(tgt.nas) == 0)
    warning("No case has missing values. Stopping as there is nothing to do.")

  xcomplete <- dm[setdiff(distInit:N,nas),]
  if (nrow(xcomplete) < k)
    stop("Not sufficient complete cases for computing neighbors.")

  for (i in tgt.nas) {

    tgtAs <- which(is.na(dm[i,]))

    dist <- scale(xcomplete,dm[i,],FALSE)

    xnom <- setdiff(nomAttrs,tgtAs)
    if (length(xnom)) dist[,xnom] <-ifelse(dist[,xnom]>0,1,dist[,xnom])

    dist <- dist[,-tgtAs]
    dist <- sqrt(drop(dist^2 %*% rep(1,ncol(dist))))
    ks <- order(dist)[seq(k)]
    for(j in tgtAs)
      if (meth == 'median')
        data[i,j] <- pguIMP::centralValue(data[setdiff(distInit:N,nas)[ks],j])
    else
      data[i,j] <- pguIMP::centralValue(data[setdiff(distInit:N,nas)[ks],j],exp(-dist[ks]))
  }

  data[1:n,]
}
