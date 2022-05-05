
library(stats)
library(manipulate)

plot.beta.binomial <- function(Deaths=5, Survivors=4, alpha=1.5,
                               beta=1.5, Bayesian=TRUE, Intervals=FALSE){
    ## Calculate the distributions
    alpha <- max(alpha, .001)
    beta <- max(beta, .001)
    num.trials <- Deaths+Survivors
    p.seq <- seq(0.00001,.99999, len=500)
    likelihood <- dbinom(x=Survivors, size=num.trials, prob=p.seq)
    prior <- dbeta(x=p.seq, alpha, beta)
    param1 <- alpha+Survivors
    param2 <-  num.trials-Survivors+beta
    posterior <- dbeta(x=p.seq, param1,param2)

    ## Scale them for plotting
    scale <- .9
    likelihood <- likelihood*scale/max(likelihood)
    density.max <- max(c(prior, posterior))
    prior <- prior*scale/density.max
    posterior <- posterior*scale/density.max
    p.hat <- Survivors/num.trials        # the MLE

    if(Intervals){
        CI.x <- qbeta(p=c(.025, .975), param1, param2)
        CI.y <- dbeta(CI.x, param1, param2)*scale/density.max
        p.hat.se <-sqrt( p.hat*(1-p.hat)/num.trials)
        p.CI <- c(-1.96*p.hat.se + p.hat, 1.96*p.hat.se+p.hat)
    }

    ## Plot them
    plot(p.seq, likelihood, type="l", lwd=2, col=1, ylab=NA,
          axes=FALSE,xlab="Probability of Survival (p)", 
          xlim=c(0,1), ylim=c(0,1))
    mtext(side=3, text="Beta-Binomial", line=1, cex=2.5)
    mtext(side=2, text="Scaled Likelihood/Density", line=1)
    axis(1); box()
    points(p.hat, scale, cex=1.25, pch=16)
    if(Bayesian){
        lines(p.seq, prior, lwd=2, col=2)
        lines(p.seq, posterior, lwd=2, col=4)
    }
    par(xpd=TRUE)
    legend(x=0, y=1.05, legend=c("Likelihood", "Confidence Interval",
                        "Prior", "Posterior", "Credible Interval"),
                        col=c(1,1,2,4,4), ncol=3, lty=c(1,2,1,1,2),
                        bty="n", lwd=2)
    ## Add confidence intervals and posterior credible intervals
    if(Intervals){
        if(Bayesian){
            points(CI.x, c(0,0), pch=16, col=4)
            segments(x0=c(CI.x[1], CI.x[2],CI.x[1]), x1=c(CI.x[1],
                     CI.x[2],CI.x[2]), y0=c(0,0,0), y1=c(CI.y[1],
                     CI.y[2],0), lwd=2, lty=2, col=4)
        }
        points(x=p.CI, y=c(scale,scale), pch=16)
        lines(p.CI, y=c(scale,scale), lwd=2, lty=2)
    }

}

plot.beta.binomial(Intervals=TRUE)


manipulate(plot.beta.binomial(Deaths, Survivors, alpha, beta,
                              Bayesian, Intervals),
                              Deaths=slider(0,25),
                              Survivors=slider(1,25),
                              alpha=slider(0,15, init=1, step=.1),
                              beta=slider(0,15, init=1, step=.1),
                              Bayesian=checkbox(),
                              Intervals=checkbox())
