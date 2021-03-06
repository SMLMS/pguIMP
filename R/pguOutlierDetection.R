#' @title nnk
#' @description
#' Outlier detection using kth Nearest Neighbour Distance method
#' Takes a dataset and finds its outliers using distance-based method
#' @param x dataset for which outliers are to be found
#' @param k No. of nearest neighbours to be used, default value is 0.05*nrow(x)
#' @param cutoff Percentile threshold used for distance, default value is 0.95
#' @param Method Distance method, default is Euclidean
#' @param rnames Logical value indicating whether the dataset has rownames, default value is False
#' @param boottimes Number of bootsrap samples to find the cutoff, default is 100 samples
#' @details nnk computes kth nearest neighbour distance of an observation and based on the bootstrapped cutoff, labels an observation as outlier. Outlierliness of the labelled 'Outlier' is also reported and it is the bootstrap estimate of probability of the observation being an outlier. For bivariate data, it also shows the scatterplot of the data with labelled outliers.
#' @return Outlier Observations: A matrix of outlier observations
#' @return Location of Outlier: Vector of Sr. no. of outliers
#' @return Outlier probability: Vector of proportion of times an outlier exceeds local bootstrap cutof
#' @importFrom stats density dist quantile
#' @references Hautamaki, V., Karkkainen, I., and Franti, P. 2004. Outlier detection using k-nearest neighbour graph. In Proc. IEEE Int. Conf. on Pattern Recognition (ICPR), Cambridge, UK.
#' @author Vinay Tiwari, Akanksha Kashikar
#' @export
#' @examples
#' #Create dataset
#' X=iris[,1:4]
#' #Outlier detection
#' nnk(X,k=4)
#'
nnk=function(x,k=0.05*nrow(x),cutoff=.95,Method="euclidean",rnames=FALSE,boottimes=100)
{

  data=as.data.frame(x)
  dis=as.matrix(stats::dist(data,diag=TRUE,upper = TRUE,method=Method))
  d=c();
  for (i in 1:nrow(data)) {
    temp=dis[,i]
    d[i]=sort(temp)[k]
  }
  quanorig=stats::quantile(d,cutoff)
  #ub=quantile(d,cutoff)
  k=stats::density(d)
  aa=which(k$x<=quanorig)
  a=max(aa)
  b=which.max(k$x>=quanorig)
  f=((k$y[b]-k$y[a])/(k$x[b]-k$x[a]))*(quanorig-k$x[a])+k$y[a]

  varorig=((1-cutoff)*cutoff)/f^2


  bootubnorm=c();f=0;k=0
  for (j in 1:boottimes) {
    s=sample(1:length(d),length(d),replace = T)
    bootdata=d[s]
    bootub=stats::quantile(bootdata,cutoff)
    k=stats::density(bootdata)
    aa=which(k$x<=quanorig)
    a=max(aa)
    b=which.max(k$x>=quanorig)
    f=((k$y[b]-k$y[a])/(k$x[b]-k$x[a]))*(quanorig-k$x[a])+k$y[a]

    v=((1-cutoff)*cutoff)/f^2
    bootubstand=(bootub-stats::quantile(d,cutoff))/sqrt(v)
    bootubnorm[j]=bootubstand*sqrt(varorig)+quanorig

  }

  ub=stats::quantile(bootubnorm,cutoff)
  wh=which(d>ub)
  out=data[wh,]
  loc=wh
  p=c()                             #outlier probability
  for (i in wh) {
    p[i]=length(which(bootubnorm<d[i]))/length(bootubnorm)
  }

  if(ncol(x)==2)
  {
    Class=as.factor(ifelse(d>ub,"Outlier","Normal"))
    cols <- c("Outlier" = "red", "Normal" = "blue")

    if(rnames==TRUE)
    {
      s=subset(data,Class=="Outlier")
      gplot=ggplot2::ggplot(data,ggplot2::aes(data[,1],data[,2]))+ggplot2::geom_point(ggplot2::aes(colour=Class,pch=Class))+ggplot2::geom_text(data=s,ggplot2::aes(x=s[,1],y=s[,2],label=rownames(s)),colour="Red", hjust = "inward",check_overlap = T)+ggplot2::ggtitle("Outlier plot using kth nearest neighbour distance")+ggplot2::xlab("Variable1")+ggplot2::ylab("Variable2")+ggplot2::scale_color_manual(values=cols)

    }else
    {dd=cbind(data,1:nrow(data))
    s=subset(dd,Class=="Outlier")
    gplot=ggplot2::ggplot(data,ggplot2::aes(data[,1],data[,2]))+ggplot2::geom_point(ggplot2::aes(colour=Class,pch=Class))+ggplot2::geom_text(data=s,ggplot2::aes(x=s[,1],y=s[,2],label=s[,3]),colour="Red", hjust = "inward",check_overlap = T)+ggplot2::ggtitle("Outlier plot using kth nearest neighbour distance")+ggplot2::xlab("Variable1")+ggplot2::ylab("Variable2")+ggplot2::scale_color_manual(values=cols)
    }
    l=list("Outlier Observations"=out,"Location of Outlier"=loc,"Outlier Probability"=p[is.na(p)==F],"Scatter plot"=gplot)
  }else if(ncol(x)==3)
  {
    Class=as.factor(ifelse(d>ub,"Outlier","Usual"))

    plot=plotly::plot_ly(x=data[,1],y=data[,2],z=data[,3],type="scatter3d",mode="markers",color=Class,colors=c("Red","Blue"))

    l=list("Outlier Observations"=out,"Location of Outlier"=loc,"Outlier Probability"=p[is.na(p)==F],"3Dplot"=plot)
  }else if(ncol(x)==4)
  {
    Class=as.factor(ifelse(d>ub,"Outlier","Usual"))

    plot=plotly::plot_ly(x=data[,1],y=data[,2],z=data[,3],size = data[,4],type="scatter3d",mode="markers",color=Class,colors=c("Red","Blue"))

    l=list("Outlier Observations"=out,"Location of Outlier"=loc,"Outlier Probability"=p[is.na(p)==F],"3Dplot"=plot)
  }else
    l=list("Outlier Observations"=out,"Location of Outlier"=loc,"Outlier Probability"=p[is.na(p)==F])
  return(l)
}
