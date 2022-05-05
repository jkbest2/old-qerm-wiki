## Run the following function and then the manipulate command
## below. This only works in RStudio and the manipulate package must
## be loaded.

## Cole Monnahan | 6/2012 | monnahc@uw.edu

plot.binomial.glm <- function(num.trials, beta0, beta1,
                              make.random=TRUE, X.length=200){
    ## This function plots the distribution of a binomial GLM given an
    ## intercept and slope for the LP. This is meant to be used with
    ## the manipulate package to explore the behavior of the model and
    ## how it changes with the parameters and num.trials.

    ## The squares are colored relative to their probabilities. The
    ## points are just data that are randomly generated for that model

    X <- seq(0,10, len=X.length)
    LP <- beta0+beta1*X
    p.true <- exp(LP)/(1+exp(LP))
    plot(0,0, type="n", xlim=range(X), ylim=c(0,1), xlab="X", ylab="Y/m",
         main="Binomial GLM")
    ## Calculate the density of the binomial distribution at each X
    ## value and plot it.
    temp <- sapply(1:length(X), function(i) {
        col.val <- dbinom(x=0:num.trials, size=num.trials, prob=p.true[i])
        col.val <- 1-col.val/max(col.val)
        points(x=rep(X[i],len=num.trials+1),
               y=(0:num.trials/num.trials),col=rgb(col.val, col.val, col.val),
               pch=15) })
    lines(X, p.true, col=3, lwd=2)

    ## Generate random data and plot it as an example.
    if(make.random){
        X.obs <- sample(X, size=10)
        LP.obs <- beta0+beta1*X.obs
        p.true.obs <- exp(LP.obs)/(1+exp(LP.obs))
        p.obs <- sapply(1:length(X.obs), function(i) rbinom(n=1,
                                                            size=num.trials,
                                                            prob=p.true.obs[i]))/num.trials
        points(X.obs, p.obs, pch=16, col=2)
    }
}

## Now explore it:
library(manipulate)
manipulate(plot.binomial.glm(num.trials, beta0, beta1),
           num.trials=slider(1,100, initial=1),
           beta0=slider(-5,5, initial=5),
           beta1=slider(-5,5, initial=-1))
