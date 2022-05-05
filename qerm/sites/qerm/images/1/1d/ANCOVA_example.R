## Run the following function and then the manipulate command
## below. This only works in RStudio and the manipulate package must
## be loaded.

## Cole Monnahan | 6/2012 | monnahc@uw.edu

ANCOVA.interaction <- function(b0,b1,b2,b3, sample.size=50){
  ## This function is intended to be used for illustrating a simple
  ## ANCOVA model. Used with the manipulate() function and sliders
  ## in RStudio it allows the user to fit the model manually and
  ## explore what exactly each parameter does.

  ## Generate some data, setting a seed so it always looks the same.
  sd.sim <- 1                         # Controls the amoutn of noise
  X <- seq(0,1, len=sample.size)
  set.seed(5)
  X1.rand <- runif(sample.size, 0,1)
  X2.rand <- runif(sample.size, 0,1)
  ## The true parameter values
  b0.true <- -2;  b1.true <- 10.5
  b2.true <- 15;  b3.true <- -20

  ## Simulate data with noise and as well as calculating the model
  ## lines
  Q <- 0
  y1 <- b0.true+b1.true*X1.rand+b2.true*Q+
    b3.true*X1.rand*Q+rnorm(sample.size,0,sd.sim)
  l1 <- b0+b1*X+b2*Q+b3*X*Q
  Q <- 1
  y2 <- b0.true+b1.true*X2.rand+b2.true*Q+
    b3.true*X2.rand*Q+rnorm(sample.size, 0, sd.sim)
  l2 <- b0+b1*X+b2*Q+b3*X*Q
  ## Plot all of it
  par(mar=c(3,3,5,1))
  plot(0,0, type="n",xlim=range(X), ylim=range(c(y1,y2)),
       xlab="X", ylab="Y", axes=FALSE, ann=FALSE)
  mtext(side=3,cex=2, line=1,
        text=expression(Y==b[0]+b[1]*X+b[2]*Q+b[3]*X*Q+epsilon))
  mtext(side=1, text="X", line=1)
  mtext(side=2, text="Y", line=1)
  box()
  points(X2.rand,y2, pch=16, col=2)
  points(X1.rand,y1, pch=16, col=1)
  lines(X,l2, col=2, lwd=2)
  lines(X,l1, lwd=2)
  ## Return the data so we can run a lm() on it and see the results
  results <- data.frame(y=c(y1,y2), x=c(X1.rand, X2.rand),
                        Q=c(rep("A",sample.size), rep("B",sample.size)))
  return(invisible(results))
}


## Before looking at real data, let's play with the model a bit. I
## wrote a separate function to help do this, and it needs to be
## loaded with this command. (This file needs to be in the same
## directory as this one.

library(manipulate)
## Using the sliders, play around and try and fit the model
## yourself. Once fit, go back and move all sliders to get a feel for
## what the parameters actually do.
fake.data=ANCOVA.interaction(0,0,0,0) # returns the fake data into temp
manipulate(ANCOVA.interaction(b0, b1,b2,b3),
           b0=slider(-5,5, 0,step=.1),
           b1=slider(0,25,0,step=.1),
           b2=slider(-15,30,0,step=.1),
           b3=slider(-25,25,0,step=.1))
## This fake data is stored in fake.data, so let's fit an LM to it and
## compare to our manually fit results.
str(fake.data)
sim.lm <- lm(y~x*Q, data=fake.data)
summary(sim.lm)
