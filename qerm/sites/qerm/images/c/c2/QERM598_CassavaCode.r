
# Plot and explore data

	setwd("c:/eli/teaching/qerm598/Week9_CurveFitting")
	
	CassavaDisease<-read.csv("CassavaDisease.csv")
	n<-names(CassavaDisease)
	for(i in 1:length(n))
		assign(n[i],CassavaDisease[,i])
	
	
	pdf("./figs/CassavaData.pdf",width=8)
		par(pch=19)
		plot(Week,Cass,type="o",ylab="Proportion of infected plants")
		lines(Week,CassMaize,col=2,type="o")
		lines(Week,CassCowpea,col=3,type="o")
		lines(Week,CassMaizeCowpea,col=4,type="o")
		legend("bottomright",bty="n",
			title="Cropping Type",
			legend=c("Cassava","Cassava-Maize","Cassava-Cowpea","Cassava-Maize-Cowpea"),
			pch=19,lty=1,col=1:4)
	dev.off()
	
			
# Fit some curves to the data
	
	Model1 <- function(t,a,b,lambda)
		return(a-b*exp(-lambda*t))
	Model2 <- function(t,a,b,lambda)
		return(a/(1+b*exp(-lambda*t)))
		
	SS <- function(p)
	{
		a<-p[1]
		b<-p[2]
		lambda<-p[3]
		
		return(sum((mydata-mymodel(Week,a,b,lambda))^2))
	}
		
	# Find reasonable initial parameters?
		p0 <- c(1,1,.1)  
		
		
	# Get parameters for Model1
		mymodel<-Model1	
		p0 <- c(1,1,.1)  
		
		mydata<-Cass;				p1.hat1 <- optim(p0,SS)$par
		mydata<-CassMaize; 			p1.hat2 <- optim(p0,SS)$par
		mydata<-CassCowpea; 		p1.hat3 <- optim(p0,SS)$par
		mydata<-CassMaizeCowpea; 	p1.hat4 <- optim(p0,SS)$par
		
	# Get parameters for Model2
		mymodel<-Model2
		p0 <- c(1,10,.2)
		
		mydata<-Cass;				p2.hat1 <- optim(p0,SS)$par
		mydata<-CassMaize; 			p2.hat2 <- optim(p0,SS)$par
		mydata<-CassCowpea; 		p2.hat3 <- optim(p0,SS)$par
		mydata<-CassMaizeCowpea; 	p2.hat4 <- optim(p0,SS)$par
		
	
	pdf("./figs/CassavaFits.pdf",width=8,height=4)
	    par(pch=19,mfrow=c(1,2),cex=0.7,mar=c(4,3,3,1))
		plot(Week,Cass,xlim=c(0,max(Week)),ylim=c(0,1),
				main="Exponential Regression Growth Curves",
				ylab="Proportion Infected")
		points(Week,CassMaize,col=2)
		points(Week,CassCowpea,col=3)
		points(Week,CassMaizeCowpea,col=4)
		
		lines(0:30,Model1(0:30,p1.hat1[1],p1.hat1[2],p1.hat1[3]),col=1)
		lines(0:30,Model1(0:30,p1.hat2[1],p1.hat2[2],p1.hat2[3]),col=2)
		lines(0:30,Model1(0:30,p1.hat3[1],p1.hat3[2],p1.hat3[3]),col=3)
		lines(0:30,Model1(0:30,p1.hat4[1],p1.hat4[2],p1.hat4[3]),col=4)
		
		legend("bottomright",bty="n",
			title="Cropping Type",
			legend=c("Cassava","Cassava-Maize","Cassava-Cowpea","Cassava-Maize-Cowpea"),
			pch=19,lty=1,col=1:4)
	text(0,0.9,expression(f(t) == a - b*e^{-lambda*t}),pos=4,cex=2)
			
			
			
		plot(Week,Cass,xlim=c(0,max(Week)),ylim=c(0,1),
				main="Logistic Regression Growth Curves",
				ylab="Proportion Infected")
		points(Week,CassMaize,col=2)
		points(Week,CassCowpea,col=3)
		points(Week,CassMaizeCowpea,col=4)
		
		lines(0:30,Model2(0:30,p2.hat1[1],p2.hat1[2],p2.hat1[3]),col=1)
		lines(0:30,Model2(0:30,p2.hat2[1],p2.hat2[2],p2.hat2[3]),col=2)
		lines(0:30,Model2(0:30,p2.hat3[1],p2.hat3[2],p2.hat3[3]),col=3)
		lines(0:30,Model2(0:30,p2.hat4[1],p2.hat4[2],p2.hat4[3]),col=4)
		
		legend("bottomright",bty="n",
			title="Cropping Type",
			legend=c("Cassava","Cassava-Maize","Cassava-Cowpea","Cassava-Maize-Cowpea"),
			pch=19,lty=1,col=1:4)
	text(0,0.9,expression(f(t) == frac(a,1 + b*e^{-lambda*t})),pos=4,cex=2)
				
	dev.off()	
	
	library(xtable)	
	xtable(data.frame(C=p1.hat1,C.M=p1.hat2,C.P=p1.hat3,C.M.P=p1.hat4,row.names=c("a","b","lambda")))
	xtable(data.frame(C=p2.hat1,C.M=p2.hat2,C.P=p2.hat3,C.M.P=p2.hat4,row.names=c("a","b","lambda")))
	
	
	




# Moving On To Whitefly Data

			
	WhiteFly<-read.csv("WhiteFly.csv")
	n<-names(WhiteFly)
	for(i in 1:length(n))
		assign(n[i],WhiteFly[,i])

	pdf("./figs/WhiteFly.pdf",width=8)
	par(pch=19)
		plot(WF.Week,WF.Cass,type="o",ylab="Number of flies",xlab="Week")
		lines(WF.Week,WF.CassMaize,col=2,type="o")
		lines(WF.Week,WF.CassCowpea,col=3,type="o")
		lines(WF.Week,WF.CassMaizeCowpea,col=4,type="o")
		legend("topright",bty="n",
			legend=c("Cassava","Cassava-Maize","Cassava-Cowpea","Cassava-Maize-Cowpea"),
			pch=19,lty=1,col=1:4)
	dev.off()
	
	
# Plot both
	pdf("./figs/CassavaWhiteFly.pdf",width=8)
		par(pch=19)
		plot(Week,Cass,type="o",ylab="",xlab="",ylim=c(0,1))
		lines(WF.Week,WF.Cass/max(WF.Cass),type="o",col=2)

		axis(4,at=seq(0,1,len=6),labels=round(seq(0,max(WF.Cass),len=6)),
			col.axis="red")
		legend("bottomright",bty="n",
			legend=c("Cassava Infection","Whitefly Numbers"),
			pch=19,lty=1,col=1:2)
	dev.off()
	

# Dynamic model 


	
	SimCass <- function(a = .1,		#infection rate
					b = .1,		#attraction rate
					c = .01,	#mortality rate
					tmax=1000,
					plotme=1)
	{
		I <- rep(0,tmax)
		N <- rep(0,tmax)
	
		for(i in 2:tmax)
		{
			I[i] <- (1-I[i-1])*N[i-1]*a + I[i-1]
			N[i] <- (1-I[i-1])*b + (1-c)*N[i-1]
			if(plotme)
			{
				plot(I[1:i],type="o",ylim=c(0,1))
				points(N[1:i]/max(N),type="o",col=2)
				axis(4,at=seq(0,1,length=11),label=round(seq(0,max(N),length=11)),col=2)
			}
		}
		return(data.frame(I,N))
	}
	
	plotCass <- function(p)
	{
		Sim.hat <- SimCass(p[1],p[2],p[3],max(Week),plotme=0)
		
		plot(Week,Cass,type="o",ylim=c(0,1),xlim=c(0,30))
		lines(WF.Week,WF.Cass/max(WF.Cass),type="o",col=2)
		
		lines(Week+t0,Sim.hat[Week,1],col=3,lty=2)
		lines(WF.Week+t0,Sim.hat[WF.Week,2]/max(Sim.hat[WF.Week,2]),col=3,lty=2)
		return(SS(p))
	}
	
	SS <- function(params)
	{
		a<-params[1]
		b<-params[2]
		c<-params[3]
		
		tmax<-max(Week)
		
		mySim <- SimCass(a,b,c,tmax,plotme=0)
		
		sim.I <- mySim$I/max(mySim$I)
		sim.N <- mySim$N/max(mySim$N)
		
		sim.I.matched <- sim.I[Week-t0]
		sim.N.matched <- sim.N[WF.Week-t0]

		data.I <- Cass/max(Cass)
		data.N <- WF.Cass/max(WF.Cass)
		
		SS.I <- sum((sim.I.matched-data.I)^2)
		SS.N <- sum((sim.N.matched-data.N)^2)
		
		return(SS.I+fudge*SS.N)
	}
	
	plotCass <- function(p)
	{
		Sim.hat <- SimCass(p[1],p[2],p[3],max(Week),plotme=0)
		
		plot(Week,Cass,type="o",ylim=c(0,1),xlim=c(0,30))
		lines(WF.Week,WF.Cass/max(WF.Cass),type="o",col=2)
		
		lines(Week,Sim.hat[Week-t0,1],col=3,lty=2)
		lines(WF.Week,Sim.hat[WF.Week-t0,2]/max(Sim.hat[Week,2]),col=3,lty=2)
		return(SS(p))
	}
	
	
	# Perform optimization
	
		t0<-3
		fudge<-0.1
		
		a<-SimCass(a=.1,b=.1,c=.5,plotme=0,tmax=100)
		plot(a[,1],type="o",ylim=c(0,1))
		points(a[,2]/max(a[,2]),type="o",col=2)
			
		p0 <- c(0.1,0.5,.4)
		p <- optim(p0,SS)$p
		
		Sim.hat<-SimCass(p[1],p[2],p[3],max(Week))
		plotCass(p)
	
	
	
	
	pdf("./figs/FittedModel.pdf",width=8)
	par(pch=19)
		plot(Week,Cass,ylim=c(0,1),xlim=c(0,30),lty=2)
		lines(Week,Cass,lty=2,col="grey")
		
		points(WF.Week,WF.Cass/max(WF.Cass),col=2,lty=2)
		lines(WF.Week,WF.Cass/max(WF.Cass),col="grey",lty=2)
		
		lines((1:dim(Sim.hat)[1]+t0),Sim.hat[,1],col=1,lty=1)
		lines((1:dim(Sim.hat)[1]+t0),Sim.hat[,2]/max(Sim.hat[,2]),col=2,lty=1)
		axis(4,at=seq(0,1,len=6),labels=round(seq(0,max(WF.Cass),len=6)),
			col.axis="red")
	dev.off()
	
	