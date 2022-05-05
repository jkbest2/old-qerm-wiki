
## This script file came from a 458 lab that I helped run in Spring
## 2012. We were exploring predator-prey dynamics so I coded up the
## model so that the students could play with the inputs to see how
## things changed. Of particular interest was the parameter "c" which
## controls the level of density dependence for the prey (the
## predators don't have any).

## The parameter names and fixed values come straight from the lab, so
## sorry about the poor naming. Also, I ran out of time to comment on
## much. One thing to note is that the isoclines are calculated
## numerically since they are challenging to do analytically. Also,
## the trajectory changes color based on the time step which is
## intended to give the user a sense of how fast the line is changing
## over time (e.g. long stretches of the same color are big jumps).

## Obviously it needs a lot of work but should get you started and
## it's fun to play with. If you make any updates to it let me know
## I'd love to see them.

## Execute the entire file and then explore the model. Must be done in
## RStudio w/ manipulate package loaded.

## Cole, monnahc@uw.edu | 6/2012


plot.model <- function( num.years,W0, L0, c, x.max=10, y.max=10,
                       r.W=.2, K.W=1.5e+06, surv.L = .8,
                       kill.rate=.000016, a=.05, area=90000,
                       a.prime=1460, psubc=.001, handle=5/365,
                       time.step=1){
    ## Sorry for the lack of comments!
    W <- rep(NA, len=num.years)
    L <- rep(NA, len=num.years)

    W[1] <- W0
    L[1] <- L0

    for(t in 2:num.years){
        Wt <- W[t-1]
        Lt <- L[t-1]
        density.W <- Wt/area
        killed <- Lt*a.prime*psubc*density.W/(1+handle*a.prime*psubc*density.W)
        recruits <- killed/(1/a+killed/c)
        W[t] <- Wt+Wt*r.W*(1-Wt/K.W)*time.step-killed
        L[t] <- Lt*surv.L+recruits
    }

    L2 <- L/1000
    W2 <- W/100000

    L3 <- L2/max(L2)
    W3 <- W2/max(W2)
    par(mfrow=c(1,2), mar=c(4,2,0,.5),oma=c(0,0,.5,0), cex.axis=.75)
    plot(x=1:num.years, y=W2, type="l",ann=FALSE, ylab=NA,
         lwd=2.5, ylim=c(0, y.max))
    mtext("Year", side=1, line=1.75)
    par(new=TRUE)
    plot(x=1:num.years, y=L2, type="l", axes=FALSE, ann=FALSE, col=2,
         lwd=2.5, ylim=c(0, y.max))
    legend("topright", legend=c("Wildebeest (x 100,000)",
                       "Lion (x 1,000)"), lwd=2, col=1:2, bg="white", cex=.75)
    axis(4, las=2);                     #mtext("L", side=4, line=2.5)

    plot(W2,L2, type="n", lwd=2,  ylim=c(0, y.max),
         axes=FALSE, ann=FALSE, xlim=c(0,x.max))
    axis(1); box()
    mtext("Wildebeest (x 100,000)", side=1, line=1.75)

    col.seq <- seq(0,1, len=num.years)
    my.cols <- rgb(1-col.seq, col.seq, 0)
    ##    my.cols <- rgb(1-W3, mean(c(1-W3, 1-L3)),1- L3)
    segments(x0=W2[-num.years], x1=W2[-1], y0=L2[-num.years], y1=L2[-1],
             lwd=2, col=my.cols)
    arrows(x0=W2[1], x1=W2[2], y0=L2[1], y1=L2[2],
           length=.075, lwd=2, col=1)

    points(W2[1], L2[1], pch=16, cex=1, col=2)
    W.seq <- seq(min(W),max(W), len=100)
    L.seq <- seq(min(L), max(L), len=100)
    plot.isoclines(W0=W0, L0=L0, c=c, L.seq=L.seq, W.seq=W.seq)
}


plot.isoclines <- function(W0, L0, c, num.isocline=100, L.seq, W.seq){

    W.isocline <- L.isocline <- matrix(NA, nrow=num.isocline, ncol=2)
    W.isocline[,1] <- W.seq/100000
    for(i in 1:num.isocline){
        W0.temp <- W.seq[i]
        W.isocline[i,2] <-
            optimize(isocline.W, interval=c(5,1e10), W0=W0.temp, c=c)$minimum
    }

    L.isocline[,1] <- L.seq/1000
    for(i in 1:num.isocline){
        L0.temp <- L.seq[i]
        L.isocline[i,2] <-
            optimize(isocline.L, interval=c(5,1e15), L0=L0.temp,c=c)$minimum
    }

    W.isocline[,2] <- W.isocline[,2]/1000
    L.isocline[,2] <- L.isocline[,2]/100000
    lines(L.isocline[,2] , L.isocline[,1])
    lines(W.isocline)
}

model <- function (num.years,W0, L0, c, r.W=.2, K.W=1.5e+06,
                   surv.L = .8, kill.rate=.000016, a=.05, area=90000,
                   a.prime=1460, psubc=.001,  handle=5/365,
                   time.step=1){

    W <- rep(NA, len=num.years)
    L <- rep(NA, len=num.years)

    W[1] <- W0
    L[1] <- L0

    for(t in 2:num.years){
        Wt <- W[t-1]
        Lt <- L[t-1]
        density.W <- Wt/area
        killed <- Lt*a.prime*psubc*density.W/(1+handle*a.prime*psubc*density.W)
        recruits <- killed/(1/a+killed/c)
        W[t] <- Wt+Wt*r.W*(1-Wt/K.W)*time.step-killed
        L[t] <- Lt*surv.L+recruits
    }
    return(cbind(W,L))
}

isocline.W <- function(L0, W0,c){
    result <- model(num.years=2, W0=W0, L0=L0, c=c, r.W=.2, K.W=1.5e+06,
                    surv.L = .8, kill.rate=.000016, a=.05, area=90000,
                    a.prime=1460, psubc=.001,  handle=5/365,
                    time.step=1)
    return(as.numeric(abs(result[1,1]-result[2,1])))
}

isocline.L <- function(W0, L0,c){
    result <- model(num.years=2, W0=W0, L0=L0, c=c, r.W=.2, K.W=1.5e+06,
                    surv.L = .8, kill.rate=.000016, a=.05, area=90000,
                    a.prime=1460, psubc=.001,  handle=5/365,
                    time.step=1)
    return(as.numeric(abs(result[1,2]-result[2,2])))
}


library(manipulate)
manipulate(plot.model(num.years, W0, L0,c, x.max, y.max),
           num.years=slider(10,1000, initial=50),
           W0=slider(5, 1e6, initial=100000),
           L0=slider(5, 30000, initial=1000),
           c=slider(500, 150000, initial=10000),
           x.max=slider(1,50, initial=10),
           y.max=slider(1,50, initial=20))
