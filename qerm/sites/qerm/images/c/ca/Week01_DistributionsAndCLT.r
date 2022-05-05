#############################
# Probability Distributions  
# QERM 598: Lab 1
# Eli Gurarie
# January 8, 2008
#############################


# There is a family of functions related to probability distribution functions.  They look like: "ddist", "pdist", "qdist", "rdist".  We'll see how they work with examples:

# 1)  "d" stands for density  
		dnorm(0)			# yields the value of the standard normal (0,1) pdf (which is equal to ....)
		dnorm(0,mean=1,sd=2)	# change parameter vales
		
	# one way to plot this function
		X <- seq(-4,6,.01)		# create a vector of x-values
		Y1 <- dnorm(X)			# apply the function to the x-values
		Y2 <- dnorm(X,1,2)		# Try different values for the mean and variance
			
	#Note that you don't actually have to specify "mean=" and "sd="
	
		plot(X,Y1,type="l")
		lines(X,Y2)				# adds a line to a given plot
		lines(X,Y2,col=2)		#if you prefer
		
	# some basic cosmetic features of "plot" 
		plot(X,Y1,type="l",xlab="X",ylab="Density",
			main="Plots of Normal Distribution density functions")
		lines(X,Y2,col=2)		
		
	# A shortcut to drawing simple functions
		curve(dnorm(x))
		curve(dnorm(x,1,2),add=T,col=2)		#"add=T" means don't draw a new plot
	
	# In-Class Assignment 1: Look up the "integrate" function (using "?") and confirm that "dnorm" yields a legitimate distribution.  
	
	
	
# 2) "p" stands for probability 

		pnorm(0)  		# gives the value of the cdf, i.e. the probability that a N(0,1) variable is *less* than 0
		pnorm(0,1,2)		

		curve(pnorm(x))
		curve(pnorm(x),xlim=c(-3,6)) 	# control limits
		curve(pnorm(x,1,2),add=T,col=2) # "add=T" means don't redraw plot, "col=2" is red

	# Consider a commonly encountered number:
		pnorm(-1.96)
		pnorm(1.96)
		
	# compare these:
		pnorm(1.96,lower.tail=F)
		1-pnorm(1.96)

	# What's the "p"-value of this number in a different distribution?
		pnorm(-1.96,1,2)
		pnorm(1.96,1,2)
		
	# Save these values
		lo1 <- pnorm(-1.96)
		hi1 <- pnorm(1.96)
		lo2 <- pnorm(-1.96,1,2)
		hi2 <- pnorm(1.96,1,2)
		
	# lets add some points to our distribution plots
		points(c(-1.96,1.96),c(lo1,hi1))
		points(c(-1.96,1.96),c(lo2,hi2),col=2,pch=2)  # pch = point style.  See bottom of "?point" for details 

	# In-Class Assignment 2: Use the "integrate" function to emulate the "pnorm" function. 
	
	
# 3) "q" stands for quantile

		qnorm(0.5) 			# returns the value for a N(0,1) variable which corresponds to the 0.5 quantile or 50'th percentile
		qnorm(0.025)
		qnorm(0.975)
		
	#so what's the 95% interval around distribution 2?
		qnorm(0.025,1,2)
		qnorm(0.975,1,2)
		
	# illustrate the intervals
		Interval1<-qnorm(c(0.025,0.975))
		Interval2<-qnorm(c(0.025,0.975),1,2)
	
		curve(dnorm(x),xlim=c(-4,6))
		curve(dnorm(x,1,2),add=T,col=2)
		
		abline(v=Interval1,lty=2)		# "abline" adds straight lines to a plot, usually with intercept (a) and slope (b).  "abline(v=..)" is a quick way to add vertical lines.  "lty=2" is a dashed line-type.  
		abline(v=Interval2,lty=2,col=2)
		
	#Let's add horizontal lines to see what the value of the density functions are at these intervals
		d1<-dnorm(qnorm(0.025))
		d2<-dnorm(qnorm(0.025,1,2),1,2)
		abline(h=d1,lty=2)
		abline(h=d2,lty=2,col=2)
		
	# This is a little too busy!   Split up the plotting window
	
		par(mfrow=c(2,1))	# par is a very very powerful and overloaded function which sets the parameters for subsequent graphics: margins, shadings, labels, colors, axis shapes, and on and on.  For now all you need to know is that the parameter "mfrow=c(nrows,ncols)" breaks the plotting window into a certain number of rows and columns.
		curve(dnorm(x))
		abline(v=Interval1,lty=2)
		abline(h=d1,lty=2)
		
		curve(dnorm(x,1,2),col=2)
		abline(v=Interval2,col=2,lty=2)
		abline(h=d2,col=2,lty=2)
		
	# It is pretty easy to do this with any number of quantiles
		Ps <- seq(0,1,.05)
		Xs <- qnorm(Ps)
		Ds <- dnorm(Xs)
		
		curve(dnorm(x),xlim=c(-3,3),lwd=2)
		abline(v=Xs)
		abline(h=Ds)
		par(mfrow=c(1,1))

	# let's get a little bit fancy 
		curve(dnorm(x),xlim=c(-2.5,2.5),lwd=2,col="grey")
		segments(x0=Xs,y0=0,x1=Xs,y1=Ds,col=3)	
		segments(x0=Xs,y0=Ds,x1=-Xs,y1=Ds,col=3)	
		# label the p-values with the text command
		text(Xs,Ds+.01,Ps,cex=0.8)  
		
	# In-class Assignment 3: Adapt the code above making a similar plot for the gamma(2,3) distribution.

	
# 4) "r" stands for random

	# This is the command that allows you to draw samples out of a distribution.  It is a one-line piece of magic that allows you to "generate data" - a trick that tends to tick off all real scientists who spend hours and hours and millions of dollars in field sites and labs painstakingly acquiring the real stuff.  
	
	# Play with it!
		rnorm(1)
		rnorm(10)
		X<-rnorm(100)
	
	# Collect statistics
		mean(X)
		sd(X)
		
	# If the "sd" command didn't exist, how would you estimate it "by hand" (in 30 characters or less)?  Is "sd" really a good name for this statistic?
	
	# More statistics and manipulations"
		max(X)
		min(X)
		range(X)
		sort(X)
		order(X)
		quantile(X)
		
	
	# plot the raw data
		plot(X)
	# plot the sorted data
		plot(sort(X))
	# plot a quantile-quantile plot
		qqnorm(X)
	# this is a very important diagnostic plot you will revisit often later
	
	# draw a histogram of the data
		hist(X)
	# Can we superimpose the known distribution?
		curve(dnorm(x),add=T) 	# doesn't work!
	
	# Two possible solutions.  #1:
		hist(X,freq=F)	# plot density, not frequency
		curve(dnorm(x),add=T,col=2)
		

	
	# Compare it to the "theoretical" distribution
		curve(dnorm(x,mean(X),sd(X)),add=T,col=3)
	
	# A little prettier:
		hist(X,freq=F,col="lightgrey",border="darkgrey",breaks=30)	
		curve(dnorm(x),add=T,col=2,lwd=1.5)
		curve(dnorm(x,mean(X),sd(X)),add=T,col=3,lwd=1.5)
		
	# maybe now's a good time to draw a legend
		legend("topleft",						# legend placement (instead of "x,y" coords)
			legend=c("known distribution","estimated distribution"),	# legend text
			col=2:3, lty=1)						# Draw lines: one red, one green
			
	# In-class assignment: Make a similar plot for the gamma(2,3) distribution!
	# This much is easy.  But how do we estimate the parameters of a gamma distribution?  See homework assignment!
	
	
	

	
# 5) Illustrating the central limit theorem
	
	# The central limit states that the sum of n iid random variables with mean mu and variance sigma converges in distribution to a Normal(mean = n*mu, var = n*sigma^2).  This is one of the most incredibly useful and powerful results since the Babylonians discovered that sweet and soggy barley spontaneously ferments into beer.  We would like to visualize it as vividly as possible.  
	
	# Consider simulated data drawn from an exponential random variable
		
		a <- rexp(1000,rate=1)
		hist(a,breaks=40,freq=F)
		curve(dnorm(x,mean=1,sd=1), # plot the CLT prediction
			add=T,col=2,lwd=2)
		
		
	# now we make a loop! (our first)
		
		for(i in 1:30) 	# this says cycle 30 times
		{
			newa <- rexp(1000)					# create new exponential data
			a<-a+newa							# sum it to the old data
			hist(a,breaks=40,freq=F)			# plot the puppy
			curve(dnorm(x,mean=i*1,sd=sqrt(i)), # plot the CLT prediction
				add=T,col=2,lwd=2)
		}

	# Whoa!  that was cool, but way too fast (and crude). 
	# Here we have to do a quick something that is completely unintuitive
	
		library(DAAG) 			# This is some random package in R that contains the function "pause()" which we want.  What else it is for, I don't know.  I don't remember how I found it.  But the "pause()" function is great. This is an example of the somewhat arbitrary nature of R.  
		
		a <- rexp(1000,rate=1)
	
		for(n in 1:30) 	
		{
			pause()
			hist(a,breaks=40,freq=F,
					main=paste("n=",n),col="grey")	# note the "paste" function
			curve(dnorm(x,mean=n,sd=sqrt(n)), 	
				add=T,col=2,lwd=2)
			newa <- rexp(1000)					
			a<-a+newa							
		}
	
	# How about fixing the x limits
	
		a <- rexp(1000,rate=1)

		for(n in 1:30) 	
		{
			pause()
			hist(a,breaks=40,freq=F,
					main=paste("n=",n),col="grey",
					xlim=c(0,40))			
			curve(dnorm(x,mean=n,sd=sqrt(n)), 	
				add=T,col=2,lwd=2)
			newa <- rexp(1000)					
			a<-a+newa							
		}
	
	# How about a discrete distribution
		p<-0.5
		a <- rbinom(1000,1,p)
		hist(a,breaks=40,freq=F,main="n=1")			# plot the puppy
		curve(dnorm(x,mean=p,sd=sqrt(p*(1-p))), 
			add=T,col=2,lwd=2)
		
		
		for(n in 1:200) 	
		{
			pause()
			hist(a,breaks=100,
					main=paste("n=",n),col="grey")			
			curve(1000*dnorm(x,mean=n*p,sd=sqrt(n*p*(1-p))), 
				add=T,col=2,lwd=2)
			newa <- rbinom(1000,1,p)
			a<-a+newa				
		}
		
		
	# How about a pathologically non-normal looking distribution?
		p <-.99
		a <- rbinom(1000,1,p)
		hist(a,breaks=40,freq=F,main="n=1")			# plot the puppy

		for(n in 1:1000) 	# this says cycle 30 times
		{
			pause()
			hist(a,breaks=100,
					main=paste("n=",n),col="grey")			# plot the puppy
			curve(1000*dnorm(x,mean=n*p,sd=sqrt(n*p*(1-p))), 	# plot the CLT prediction
				add=T,col=2,lwd=2)
			newa <- rbinom(1000,1,p)					# create new exponential data
			a<-a+newa							# sum it to the old data
		}
		



##############################
# Some Numerical Experiments #
# (thanks to M.Keim)         #
##############################


# Experiment 1: CLT
	
	# create a blank vector to save simulation results
		x.bars<-rep(NA,1000)	
	
	# create a loop to repeat the experiment
	# "i" will index the iterations, starting at 1 and ending at 1000
	
		for(i in 1:1000)				# open loop 
		{				
			x<-rnorm(5,mean=0,sd=1)		# simulate 5 standard normal variables  
			x.bars[i]<-mean(x)	# store the sample mean of the 5 rv's
		}								# close the loop 
	
	# calculate the mean of our stored sample means
		mean(x.bars)	# should be near 0 (by E(X.bar) = mu)
		
	# calculate the sample variance of the stored sample means
		var(x.bars)	# should be near 1/5 (by CLT)
	
	# create a histogram of our sample means with 50 bins/breaks/intervals.
		hist(x.bars,breaks=50,freq=F)
		
	# draw a blue line (col="blue") that is double width (lwd=2) of the "kernel density" estimated for our sample means
		lines(density(x.bars),col="blue",lwd=2)
	
	# draw a double width red line of a N(0,1/5) density curve.
		curve(dnorm(x,0,sqrt(1/5)),col="red",lwd=2,add=T)
		
	# Exercise: Draw a green line of double width for a Normal curve having mean equal to the sample mean and standard deviation equal to the sample standard deviation.
	
	
	
	# It's generally better to name and separate all your parameters at the top of your code
			
		reps<-100
		n<-3
		mu<-0
		sigma<-1
		
		x.bars<-rep(0,reps)
		for(i in 1:reps)
			x.bars[i]<-mean(rnorm(n,mean=mu,sd=sigma))
		
		mu.hat <- mean(x.bars)	# should be near ___?
		sigma.hat <- sd(x.bars)	# should be near ___?
		
		hist(x.bars,breaks=sqrt(reps),freq=F)
		lines(density(x.bars),col="blue",lwd=2)
		curve(dnorm(x,mu,sigma/sqrt(n)),col="red",lwd=2,add=T)
		curve(dnorm(x,mu.hat,sigma.hat),col="green",lwd=2,add=T)
	
	# re-run with n=30,100, num.sims=10^4
	# Adapt the above code to Poisson and Uniform distributions. 

	
	
	
#########################
# Experiment 2 with CLT #
#########################

	# How does convergence to the CLT improve with sample size?
	# Using a non-Gaussian example...

		sizes<-c(2,5,10,30)
		reps<-1000
		r<-1		# exponential rate parameter
	
	# we need to make an array (or matrix (or data.frame)) to put all of the 
		x.bars<-array(dim=c(1000,length(sizes)))
	
		for(j in 1:(length(sizes)))
		{
			n<-sizes[j]
			
			for(i in 1:reps)
				x.bars[i,j]<-mean(rexp(n,rate=r))
		}
	
	# Plot each of these histograms
	
	par(mfrow=c(2,2))
		
	for(i in 1:length(sizes))
	{
		n<-sizes[i]
		
		hist(x.bars[,i],breaks=50,freq=F,
			main=paste("n =",n),
			xlab="x.bar")
		lines(density(x.bars[,i]),
				col="blue",lwd=2)
		curve(dnorm(x,mean=r,sd=1/sqrt(n)),
				col="red",lwd=2,add=T)
	}
	
	
	
	