###
## R code for QERM 598 lecture 7, Feb 20, 2008, by Ian Taylor
#

# data on south african hake (from Andre Punt, FISH 458 lecture 3, 2005)
year = c(1917,1918,1919,1920,1921,1922,1923,1924,1925,1926,1927,1928,1929,1930,1931,1932,1933,1934,1935,1936,1937,1938,1939,1940,1941,1942,1943,1944,1945,1946,1947,1948,1949,1950,1951,1952,1953,1954,1955,1956,1957,1958,1959,1960,1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992)
CPUE = c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,17.31,15.64,16.47,16.26,16.26,17.31,12.09,14.18,13.97,14.6,10.84,10.63,10.01,10.01,8.62,7.23,7.09,4.9,4.97,4.65,4.66,5.35,4.84,5.9,6.13,5.48,5.81,5.87,6.49,6.67,7.29,6.93,6.35,6.88,7.16,7.29,7.26,7.78)
catch = c(1.0,1.1,1.9,0.0,1.3,1.0,2.5,1.5,1.9,1.4,0.8,2.6,3.8,4.4,2.8,14.3,11.1,13.8,15.0,17.7,20.2,21.1,20.0,28.6,30.6,34.5,37.9,34.1,29.2,40.4,41.4,58.8,57.4,72.0,89.5,88.8,93.5,105.4,115.4,118.2,126.4,130.7,146.0,159.9,148.7,147.6,169.5,162.3,203.0,195.0,176.7,143.6,165.1,142.5,202.0,243.9,157.8,123.0,89.6,143.9,102.3,101.1,92.7,101.5,100.7,86.0,73.7,88.4,99.6,109.1,104.0,90.1,84.9,78.9,85.5,87.2)
dat = data.frame(year=year,CPUE=CPUE,catch=catch)

# =========================================================================================

plotHake = function(expectedCPUE=0, pdfON=F, filename=F)
{
  # function to plot the data and fit of model to data
  if(pdfON) pdf(paste('/home/ian/dogshare/talks/QERM_computing_course/',filename,'.pdf',sep=''),7,5)

  par(mar=c(5,4,1,4)+.1)
  plot(dat$year, dat$catch, xlab='year',ylab='landings',type='h', lend=1, lwd=3, col='grey', yaxs='i', ylim=c(0,1.2*max(dat$catch)))

  axis2vals = pretty(c(0,max(c(dat$CPUE[!is.na(dat$CPUE)]),expectedCPUE)))
  rescale = .9*par('usr')[4]/max(axis2vals) #rescale based on 2nd axis

  points(dat$year, dat$CPUE*rescale, pch=16, col=4)
  axis(side=4, label=axis2vals, at=axis2vals*rescale)
  mtext(side=4, line=3, 'CPUE')
  if(expectedCPUE[1]!=0)
  {
    lines(dat$year, expectedCPUE*rescale, col=2, lwd=2)
    legend('topleft', legend=c('CPUE','expected CPUE','landings'), pch=c(16,NA,NA), lty=c(NA,1,1), lwd=c(NA,2,3),col=c(4,2,'grey'), bty='n')
  }else{
    legend('topleft', legend=c('CPUE','landings'), pch=c(16,NA), lty=c(NA,1), lwd=c(NA,3), col=c(4,'grey'), bty='n')
  }
  if(pdfON) dev.off() # close PDF file if printing to PDF
}

plotHake()
# =========================================================================================

fib = function(r,K,years,M1,N1,catch=0)
{
  # a popsize dynamics model for rabbits developed by Leonardo of Pisa (Fibonacci)
  # based on the work of Pingala, Hemachandra, and Gopāla
  # r is breeding success rate
  # K is carrying capacity
  # years is vector of years over which to project the model
  # M1 and N1 are the initial number of mature and new-born pairs
  # catch (optional) is a vector of removals of length = years
  nyears = length(years)
  if(length(catch)<nyears) catch=rep(catch,nyears) # if no catch vector input then repeat for n years
  M = rep(NA,nyears) # number of mature rabbit pairs
  N = rep(NA,nyears) # number of new rabbit pairs
  M[1] = M1  # initial number of mature rabbit pairs
  N[1] = N1  # initial number of new rabbit pairs
  for(i in 1:(nyears-1)){
    M[i+1] = max(0, M[i]+N[i]-catch[i]) # rabbits live forever and mature after 1 month
    N[i+1] = max(0, r*(1-M[i]/K)*M[i]   ) # r*(1-M/K) new pairs are born per mature pair
  }
  out = data.frame(y=years,M=M,N=N,total=M+N)
  return(out)
}

# putting fib function to work
fib(r=1,K=1e20,years=1:10,M1=0,N1=1)$total

x = fib(r=1,K=1e20,years=1:20,M1=0,N1=1)$total
xratios = x[-1]/x[-20]
plot(xratios, type='o')


# =========================================================================================

negloglike = function(index, popsize)
{
  # generic function giving negative log likelihood of
  # fit of index of abundance to population size
  index_good = index[!is.na(index) & !is.na(popsize)] # eliminate NA values
  pop_good = popsize[!is.na(index) & !is.na(popsize)] # eliminate NA values
  ngood = length(index_good)

  # calculate scaling factor to translate between popsize units and relative index of abundance
  qIndex = sum(index_good/pop_good)/ngood

  # under assumption of normal error, negative log-likelihood is proportional to sum of squares
  negloglike = sum((index_good - qIndex*pop_good)^2)/ngood
  if(min(pop_good) == 0) negloglike = 20 # a big penalty for a crashed population
  return(likevals = list(negloglike=negloglike, qIndex=qIndex))
}

# =========================================================================================
fitKr = function(parvec)
{
  # compute the fit of the fibonacci model to the cape hake data assuming that the
  # initial population starts at equilibrium (M1=K, N1=0)
  K = parvec[1]
  r = parvec[2]
  popKr = fib(K=K, r=r, M1=K, N1=0, years=dat$year, catch=dat$catch)$total
  negloglikeValue = negloglike(index=dat$CPUE, popsize=popKr)$negloglike

  # uniform prior on K (would be 0 outside intervals but that causes computational problems)
  Kprior = if(K >= 0 & K <= 1e6){ 1 }else{1e-20}
  rprior = if(r >= 0 & r <=   1){ 1 }else{1e-20}
  posterior = negloglikeValue - log(Kprior) - log(rprior)
  return(posterior)
}

# =========================================================================================

# maximum posterior density estimates (Bayesian version of MLE)
KrMPDE = optim(c(10000,.5),fitKr)
KrMPDE
KrMPDE = KrMPDE$par


popMPDE = fib(K=KrMPDE[1], r=KrMPDE[2], M1=KrMPDE[1], N1=0, years=dat$year, catch=dat$catch)$total
qMPDE = negloglike(index=dat$CPUE, popsize=popMPDE)$qIndex

plotHake(popMPDE*qMPDE)

# =========================================================================================


MCMC = function(Ndraws, thin=1, burnin=0, Npars=2, objfun=fitKr, parnames=c('K','r'),
  initvec=c(10000,.5), jump=c(10,.0001), Llimit=c(1,0), Ulimit=c(1e6,1))
{
  # generic metropolis-hastings algorithm to do MCMC search over
  # jump, Llimit, and Ulimit need to be of length = Npars
  # objfun is the objective function to be sampled over

  # set up timer
  startTime = Sys.time()
  # start out at MPDE
  currentdraw = optim(initvec,objfun)$par
  currentlike = fitKr(currentdraw)
  output = c(currentdraw,exp(-currentlike))
  accept = 0
  for(idraw in 1:Ndraws)
  {
    # draw new sample from jump function
    # note uniform random variables drawn using runif(n,min,max)
    newdraw = currentdraw + jump*runif(2,-.5,.5)
    for(ipar in 1:Npars)
    {
      # reflecting boundaries
      if(newdraw[ipar] > Ulimit[ipar]) newdraw[ipar] = 2*Ulimit[ipar] - newdraw[ipar]
      if(newdraw[ipar] < Llimit[ipar]) newdraw[ipar] = 2*Llimit[ipar] - newdraw[ipar]
    }

    # compute likelihood at new value
    newlike = objfun(newdraw)
    # values are negative log likelihood, but ratio is in normal space, so convert
    likeratio = exp(-newlike)/exp(-currentlike)

    # if ratio > 1, always accepted, otherwise accepted with probability equal to ratio
    if(likeratio > runif(1,0,1))
    {
      currentdraw = newdraw
      currentlike = newlike
      accept = accept+1
    }
    # save value to output only after burn in period and only when thinned
    if(idraw>burnin & idraw%%thin==0)
    {
      output = rbind(output,c(currentdraw,exp(-currentlike)))
    }

    # output some timer stats 10 times per run
    if(idraw%%floor(Ndraws/10)==0)
    {
      currTime <- round(difftime(Sys.time(),startTime,units='sec'))
      estTime <- round(currTime * Ndraws / idraw - currTime)
      print(paste('i=',idraw,' accept=',round(accept/idraw,3),', elapsed=',currTime,'sec, remaining=',estTime,'sec, total=',currTime+estTime,'sec', sep=''))
    }
  }
  # format the output for outputting
  rownames(output)=1:nrow(output) # otherwise we get a warning about repeated values
  output = data.frame(output)
  names(output) = c(parnames,'like')

  return(output)
}


x = MCMC(10000,thin=10,jump=c(100,.01))
plot(x$r,x$K)
par(mfrow=c(2,1))
plot(x$r, type='l')
plot(x$K, type='l')

hist(x$r*x$K/4, col='grey', breaks=50, xlab='MSY', ylab='posterior density')

rvec = seq(.3,.45,length=100)    # vector of r values over which to comput fit
Kvec = seq(1300,1600,length=100)   # vector of K values over which to comput fit

negloglikemat = matrix(NA,length(Kvec),length(rvec)) # empty matrix to store fit

for(irow in 1:length(Kvec))
{
  for(icol in 1:length(rvec))
  {
    negloglikemat[irow,icol] = fitKr(c(K=Kvec[irow],r=rvec[icol]))
  }
  if(irow%%10==0) print(paste('irow = ',irow,'/',length(Kvec),sep=''))
}

likemat = exp(-negloglikemat)
#likemat[likemat==1000] = NA
#likemat[is.na(likemat)] = 0
image(Kvec,rvec,likemat)
persp1 = persp(Kvec,rvec,likemat, theta=-30,phi=30)
points(trans3d(x$K,x$r,x$like, persp1), pch=16, col=colvec[4])
