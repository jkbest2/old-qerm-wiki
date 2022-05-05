#####################
# QERM 598			#
# Lab 4 1.30.2008	#
#####################


##################################################
# PART 1: Sleep-Concentration Data

	# We are going to do a quick linear regression on the Sleep-Concentration experiment described in the notes
	
	#First, load the data, look at it, extract the vectors:
		Sleep.df <- read.table("./Sleep.dat",header=T)
		Sleep.df
		Sleep <- Sleep.df$Sleep
		ZOD <- Sleep.df$ZOD
	
	# plot the data
		plot(Sleep,ZOD,
				xlab="Sleep (hrs)",
				ylab="Zone-out duration (min)")
				
	# Now we gather the pieces we need for our estimates
	# Recall our slope estimate alpha.hat = sum(X_i - X_bar)(Y_i - Y_bar) / sum(X_i - X_bar)^2
		Sleep.bar <- mean(Sleep)
		ZOD.bar <- mean(ZOD)	
		alpha.hat <- sum( (Sleep - Sleep.bar) * (ZOD - ZOD.bar) )/ sum( (Sleep - Sleep.bar)^2 )
		
	# and the intercept estimate beta.hat = y.bar - alpha.hat x.bar
		beta.hat <- ZOD.bar - alpha.hat*Sleep.bar
		
	# add the estimated line to the plot.   The "abline()" adds lines to plots when given a slope and intercept in that order.
		abline(c(beta.hat,alpha.hat))
		
	# obtain an estimate for sigma^2 = MSE = SSE/(n-2).  To do this, we must obtain the theoretical values of our model
		ZOD.theory <- beta.hat + alpha.hat * Sleep
		SSE <- sum((ZOD-ZOD.theory)^2)
		MSE <- SSE / 13
		S
	# Recall our test statistic:  alpha.hat / s(alpha), where s(alpha) = MSE/sum(X_i-X.bar)^2
		s2.alpha <- MSE/sum((Sleep - Sleep.bar)^2)
		t0 <- alpha.hat/sqrt(s2.alpha)
	
	# Obtain a p-value 
		P <- pt(t0,13)*2
		
	# Why times 2?  Why is the following a slightly better way to obtain a p-value?
		P <- 2*(1-pt(abs(t0),13))
		
	# Now, for the one-line-of-code method:
		Sleep.lm <- lm(ZOD ~ Sleep)
		summary(Sleep.lm)
		anova(Sleep.lm)
	# do the computer's values agree with yours?
	
	# Note that the "lm" object you created can be passed directly to the "abline()" function
		plot(Sleep,ZOD,
				xlab="Sleep (hrs)",
				ylab="Zone-out duration (min)")
		abline(Sleep.lm)
	
	# Don't forget to assess your model assumptions:
		plot(Sleep.lm)

	
##################################################
# PART 2: Sleep-Pie-Concentration Data

	# Read the Pie-Sleep data a
		PieSleep.df <- read.table("./PieSleep.dat",header=T)
		PieSleep.df
		Pie <- PieSleep.df$Pie
		Sleep <- PieSleep.df$Sleep
		ZOD <- PieSleep.df$ZOD
		
	# make a boxplot, look at the means, do a simple single-factor anova 
		boxplot(ZOD~Pie)
		tapply(ZOD,Pie,mean)
		anova(lm(ZOD~Pie))
		
			
	# Make a grand data-plot?
		plot(Sleep,ZOD)	
		cols<-as.integer(Pie)
		plot(Sleep,ZOD,col=cols)
		
	# add a legend.  For a legend, note that you have to specify the point or line type. 
		legend(x=2,y=6,
				legend=c("A","B","C"),
				col=1:3,
				pch=1)
	
	# add some linear regression lines
		PieSleep.lmA <- lm(ZOD[Pie=="A"]~Sleep[Pie=="A"])
		PieSleep.lmB <- lm(ZOD[Pie=="B"]~Sleep[Pie=="B"])
		PieSleep.lmC <- lm(ZOD[Pie=="C"]~Sleep[Pie=="C"])
		
		abline(PieSleep.lmA,col=1)
		abline(PieSleep.lmB,col=2)
		abline(PieSleep.lmC,col=3)
		
	# Do a two-factor anova with all the possible interactions.  
	# The 'X * Y' notation means consider main X and Y effects AND the XY interaction effect
		PieSleep.lm <- lm(ZOD ~ Pie * Sleep)
		anova(PieSleep.lm)
		
	# Select a more parsimonious model.
	# Here, 'X + Y' means, consider only the main effects
		PieSleep.lm <- lm(ZOD ~ Pie + Sleep)
		anova(PieSleep.lm)
		
	# What has happened to the p-values?
	# Look at the parameters from the model you've selected.  
		PieSleep.lm
	# How do you translate these numbers in terms of our, more `standard' notation
		
		