plotgreys <- function(mat,vec,xlim='default',ylim='default',
                      lwd=c(1,3,1),lty=c(3,1,3),new=T,...)
{
  # function written by Ian Taylor to shade distributions of time-series values,
  # such as abundance over time for many MCMCs or boostrap samples
  #
  # shading is a range of opaque greys from light to dark
  #
  nvals <- length(vec)
  CIlevels = seq(.05,.95,.1) #Probability intervals
  Nlevels = length(CIlevels)
  CIprobs = sort(c(1 - CIlevels/2, CIlevels/2)) #quantiles containing the prob. intervals.

  quant = apply(mat,1,quantile, probs=c(.025,.5,.975)) #get median and 95% values
  
  quant2 = apply(mat,1,quantile, probs=CIprobs) #get values for all quantiles
  ##plot(0, type='n', xlim=range(vec), ylim=c(0,1.1*max(quant)), xaxs='i', yaxs='i', xlab='', ylab='', main='')

  if(xlim=='default') xlim=range(vec)
  if(ylim=='default') ylim=range(quant2)

  if(new) plot(0, type='n', xlim=xlim, ylim=ylim, ...)
  for(ilevel in 1:Nlevels){
    polygon(vec[c(1:nvals,nvals:1)],
            c(quant2[ilevel,1:nvals],
              quant2[2*Nlevels+1-ilevel,nvals:1]),
            border=NA, col=paste('grey',floor(45+50*CIlevels[Nlevels+1-ilevel])))
  }
  matplot(vec,t(quant), type='l', col=1, lwd=lwd, lty=lty, add=T)
}

probshade <- function(mat,vec,xlim='default',ylim='default',
                      lwd=c(1,3,1),col='red',alpha=.1,lty=c(3,1,3),new=T,...)
{
  # function written by Ian Taylor to shade distributions of time-series values,
  # such as abundance over time for many MCMCs or boostrap samples
  #
  # shading uses alpha transparency to allow overlaid plots with blended colors
  #
  nvals <- length(vec)
  CIlevels <- seq(.05,.95,.1) #Probability intervals
  Nlevels <- length(CIlevels)
  CIprobs <- sort(c(1 - CIlevels/2, CIlevels/2)) #quantiles containing the prob. intervals.

  if(is.character(col)) col <- as.numeric(col2rgb(col))/255

  colvec <- rep(rgb(col[1],col[2],col[3],alpha=alpha),Nlevels)
  #    ramp <- colorRamp(c(col, "white"))
  #    colvec <- rgb(ramp(1-CIprobs), max=255)

  quant <- apply(mat,1,quantile, probs=c(.025,.5,.975)) #get median and 95% values
  quant2 <- apply(mat,1,quantile, probs=CIprobs) #get values for all quantiles
  ##plot(0, type='n', xlim=range(vec), ylim=c(0,1.1*max(quant)), xaxs='i', yaxs='i', xlab='', ylab='', main='')

  if(xlim=='default') xlim <- range(vec)
  if(ylim=='default') ylim <- range(quant2)

  if(new) plot(0, type='n', xlim=xlim, ylim=ylim, ...)
  for(ilevel in 1:Nlevels){
    polygon(vec[c(1:nvals,nvals:1)],
            c(quant2[ilevel,1:nvals],
              quant2[2*Nlevels+1-ilevel,nvals:1]),
            border=NA, col=colvec[ilevel])
  }
  matplot(vec,t(quant), type='l', col=1, lwd=lwd, lty=lty, add=T)
}
