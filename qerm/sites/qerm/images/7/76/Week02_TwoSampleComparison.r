#############################
# Two Sample Comparisons
# QERM 598: Lab 2
# Eli Gurarie
# January 8, 2008
#############################



##########################
# 1) Loading Data into R #
##########################
	
	# There are several ways of loading data into R.  
	# Entering data in by hand is the simplest and least generally useful method:
	
		Weight<-c(51,55,53,48,31,72,45,65,50,102,57,38,67,57,76,67,43,50,35,65,41,63,48,59,44,60,48,52,51,47,90,104,106,57,90,132,91,110,86,152,74,58,71,79,67,112,103,61,141,81,103,56,81,91,130,59,91,108,125,75)
	
	# Hopefully that's the first and last time you ever do that.  More generally, data will be in text files that you will load directly into the memory of R.  
	
	# The simplest file loading command is "scan()", which loads a single vector from a specified text file.  In order to be able to access a file, you need to know how to navigate textually within your folder system.  Thus:
	
	Weight <- scan("c:/eli/teaching/QERM598/datasets/Weight.dat")
	
	# In MacOS or Unix you define your directory building from your user home directory
	
	# The following *can* work:
	
		Weight <- scan("Weight.dat")
	
	# but only if your file is in the working directory.  What is the working directory?  Use this command:
	
		getwd()	
	
	# to find out, and this command:
	
		setwd("c:/eli/teaching/qerm598/datasets")
	
	# to change it. Now you can access anything in that folder immediately.
	
	# scan() is obviously limited.  Usually data will be in big tables, that might look like this:
	#	
	#	Colony	Distance	Weight	Headwidth	Worker class
	#	28	0	62	.642	39-40
	#	28	0	53	.642	39-40
	#	28	0	51	.6		37-38
	#	28	0	43	.516	<37
	#	28	0	65	.684	39-40
	#	28	0	64	.684	39-40
	#	28	0	68	.684	39-40
	#	28	0	45	.558	37-38
	#	28	0	66	.684	39-40
	#	28	0	62	.642	39-40
	
	# The command to load a table is:
	
		Ants <- read.table("ants.dat",header=T)
		
	# The "header=T" means: take the first row of the datatable and have that be the name of the subsequent columns. 
	
	# A function I've come to use a lot that is almost identical to the one above is "read.csv".  I find managing data in a spreadsheet program like Excel and saving it as a "csv" (comma-separated v...) is most convenient both for preprocessing and loading into R
	
		Ants <- read.csv("ants.csv",header=T)
		
###############################
# 2) Manipulating data-tables #
###############################
	
	# The complete ant-size and behavior dataset is enormous.  For this lab, each person is going to randomly sub-sample the data (30 of each kind of ant) and analyze the data.  
	
	# "names()" gives the name of the columns
		names(Ants)
	
	# "$" accesses any given column
		Ants$Weight
		
	# compare to:
		Ants[,4]
		
	# save some columns as separate vectors
		Species <- Ants$Species
		Weight <- Ants$Weight
		Head <- Ants$Headwidth
		
	# Note that unlike "Weight" and "Headsize", "Species" is a vector of factors with two "levels"
		levels(Species)
		
	# We use Species to subset the data
		Weight.T <- Weight[Species=="Thatch"]
		Weight.S <- Weight[Species=="Seed"]
		 Head.T <- Head[Species=="Thatch"]
		 Head.S <- Head[Species=="Seed"]
		

#######################################
# 3) Exploratory analysis: Total Data #
#######################################
	
	# boxplot
		par(mfrow=c(1,2))
		boxplot(Weight~Species)
		boxplot(Head~Species,col=2:3)
		
	# The notation "A~B" means "break up A in terms of the factor levels of B".  This is very useful notation that has lots of more sophisticated applications later.
	
	# some histograms
		par(mfrow=c(2,2))
		hist(Weight.T,col=1)
		hist(Weight.S,col=2)
		hist( Head.T ,col=3)
		hist( Head.S ,col=4)
	# any comments on normality?
	
	# Just for fun: is there a relationship between Weight and Headwidth?
		plot(Weight,Head)
	# What to we notice about this data?.. Note in particular the spacing of head sizes.
	
	
	# A quick way to get summary stats that might at first appear confusing but is in fact incredibly useful is "tapply()"
		tapply(Weight,Species,mean)
		tapply(Head,Species,mean)
		
		tapply(Weight,Species,sd)
		tapply(Head,Species,sd)
		
		tapply(Weight,Species,length)
		tapply(Head,Species,length)
	# and so on. Can you figure out what this does?
		
	
	
	# - But Wait!  before we go any further, we need to subsample this population.  For this we use the very useful "sample" function.  
		 Weight.T <-  sample(Weight.T)[1:30] 
		 Weight.S <-  sample(Weight.S)[1:30] 
	     Head.S  <-  sample( Head.S )[1:30] 
	     Head.T  <-  sample( Head.T )[1:30]
	      
	# And reconstruct the Head, Weight and Species vectors
		Species <- c(rep("Thatch",30),rep("Seed",30))
		Weight<-c(Weight.T,Weight.S)
		Head<-c(Head.T,Head.S)
	# Is there still a relationship between Head and Weight?  Where did it go?


###########################################
# 4) Exploratory analysis 2: Sampled data #
###########################################       

	# Plots again, but pretty!

	col<-c("turquoise","turquoise4","salmon","salmon4")

	# boxplots
	
	par(mfrow=c(1,2))	
	boxplot(Weight~Species,main="Weight (mg)",col=col[1:2])
	boxplot(Head~Species,main="Head widths (mm)",col=col[3:4])

	# histograms
		# first collect extrema so that the axes have similar ranges
	
		w.min <- min(Weight)
		w.max <- max(Weight)
		h.min <- min(Head)
		h.max <- max(Head)
		
	par(mfrow=c(2,2),cex=0.7)
	hist(Weight.S,main="Weight: Seed Ant",
		breaks=seq(w.min,w.max+10,5),
		col=col[1],
		xlab="Weight (mg)")
		
	hist(Head.S,main="Head width: Seed Ant",
		breaks=seq(h.min,h.max+.1,.05),
		col=col[3],
		xlab="Head width (mm)")
		
	hist(Weight.T,main="Weight: Thatch Ant",
		breaks=seq(w.min,w.max+10,5),
		col=col[2],
		xlab="Weight (mg)")

	hist(Head.T,main="Head width: Thatch Ant",
		breaks=seq(h.min,h.max+.1,.05),col=col[4],
		xlab="Head width (mm)")

	# summarystats (we'll need these later)
		
		Wbar.T <-   mean(Weight.T)
		Wbar.S <-   mean(Weight.S)
		Hbar.T  <-  mean( Head.T )
		Hbar.S  <-  mean( Head.S )
		
		Wsd.T <-   sd(Weight.T)
		Wsd.S <-   sd(Weight.S)
		Hsd.T  <-  sd( Head.T )
		Hsd.S  <-  sd( Head.S )

	# Make total comparison matrix!
	
		Weight.Diff <- matrix(NA,length(Weight.S),length(Weight.T))
		for(i in 1:length(Weight.S))
			Weight.Diff[i,] <- Weight.T - Weight.S[i]
			
	# Of all comparisons, how many are greater than zero?  I.e, what is the C statistic?
		
		sum(Weight.Diff>0)/length(Weight.Diff)
		
	
	# this handy function plots a color-gradated image
		image(Weight.Diff)
		
	# what can you do to make it look more like the one in the presentation?
		Weight.Diff <- matrix(NA,length(Weight.S),length(Weight.T))
		for(i in 1:length(Weight.S))
			Weight.Diff[i,] <- sort(Weight.T) - sort(Weight.S)[i]	
	
	# Once you've done that, you can add contours immediately:
		image(Weight.Diff)
		contour(Weight.Diff,add=T)
	
	# and just for fun, make a pdf
		pdf("c:/GratuitousComparisonPlot.pdf")
			image(Weight.Diff)
			contour(Weight.Diff,add=T)
		dev.off()
		

		
#######################################################
# 5) Perform Monte-Carlo null-distribution generation #
#######################################################

	# Recall, our test statistic is: T = mean(X_T) - mean(X_W)
	# the "observed" T-statistic (i.e. the summary of the actual data) is:
	
		T.obs <- mean(Weight.T)-mean(Weight.S)
	
	# A typical value: 38.2 .... the quetion is: is this extreme?
	# To answer it, we need to generate a null-distribution

		nreps<-10000
		T.sim<-rep(NA,nreps)
		
		for(i in 1:nreps)
		{ 
			# shuffle all the Weights
				W.sim <- sample(Weight)
			# sample out 30 "Seed" ants and 30 "Thatch" ants
				W.sim.Seed <- W.sim[1:30]
				W.sim.Thatch <- W.sim[31:60]
			# collect statistics
				T.sim[i] <- mean(W.sim.Seed)-mean(W.sim.Thatch)
		}
	
	# Plot a histogram of the null-distribution
		hist(T.sim)
	# Where does the observed t-value lie relative to the simulated dsitribution?
		
	# Obtain a approximate p-value for the statistic
		# one-sided
		sum(T.sim>abs(T.obs))
		
		# two-sided
		sum(abs(T.sim)>abs(T.obs))
		
	# In both cases, the answer will probably be 0, indicating that the difference in the Weight means truly is extreme.  and we can reject the Null Hypothesis of equality with high confidence.  
		
	# The results for headwidth are somewhat more ambiguous
	
		T.obs <- mean(Head.T)-mean(Head.S)
	
	# A typical value: 38.2 .... the question is: is this extreme?
	# To answer it, we need to generate a null-distribution

		nreps<-10000
		T.sim<-rep(NA,nreps)
		
		for(i in 1:nreps)
		{ 
			# shuffle all the Weights
				H.sim <- sample(Head)
			# sample out 30 "Seed" ants and 30 "Thatch" ants
				H.sim.Seed <- H.sim[1:30]
				H.sim.Thatch <- H.sim[31:60]
			# collect statistics
				T.sim[i] <- mean(H.sim.Seed)-mean(H.sim.Thatch)
		}
		# draw the null-distribution
			hist(T.sim)
		# and the observed statistic
			abline(v=T.obs)
	# This time, the difference in the means is not so dramatic, and we can have rather high p-values
		# one-sided
			sum(T.sim>abs(T.obs))
		# two-sided
			sum(abs(T.sim)>abs(T.obs))
				
	# Here's some code to make a plot which shades the part of the histogram that is outside of the observed statistic
		# One-sided
		T.obs <- mean(Head.T)-mean(Head.S)
		hist(T.sim,main=expression(bar(H[t])-bar(H[s])),
			col="grey",breaks=seq(-.2,.2,.01))
		abline(v=T.obs,col="darkred",lwd=2)
		hist(T.sim[T.sim>=T.obs],
			col="red",add=T,breaks=seq(-.2,.2,.01))
		
		# Two-sided
		T.obs <- mean(Head.T)-mean(Head.S)
		hist(T.sim,main=expression(bar(H[t])-bar(H[s])),
			col="grey",breaks=seq(-.2,.2,.01))
		abline(v=T.obs,col="darkred",lwd=2)
		hist(T.sim[T.sim>=T.obs],
			col="red",add=T,breaks=seq(-.2,.2,.01))
		hist(T.sim[T.sim<=-T.obs],
			col="red",add=T,breaks=seq(-.2,.2,.01))
			
		# It is possible that some of the first histogram bar will be filled in.  This reflects that some of the data within that interval is less extreme than the t-observed value.  If you thing that's not pretty enough, try rounding the T.obs. 
		
		
##################################
# 6. Performing a t-test by hand #
##################################

	# We will do this for the headwidths, since the weights are less interesting 

	# The t-test-statistic is given in the lecture.  We can break it down into it's component parts:
		Xbar1 <- mean(Head.T)
		Xbar2 <- mean(Head.S)
	
		n1 <- length(Head.T)
		n2 <- length(Head.S)
		S1 <- sd(Head.T)
		S2 <- sd(Head.S)
		
		# pooled variance!
		Sp <- sqrt( ((n1-1)*S1^2 + (n2-1)*S2^2 )/(n1+n2-2) )
		
	# Put it all together in the observed t-statistic
		t.obs <- abs(Xbar1 - Xbar2) / (Sp*sqrt(1/n1 + 1/n2))
		
	# The distribution of this statistic is a t-distribution with n1+n2-2 degrees of freedom, which is easy to draw:
		curve(dt(x,58))
	# as is our test value
		abline(v=t.obs)
	# the one-sided p-value is
		1-pt(t.obs,df=58)
	# the two-sided p-value is twice that
		2*(1-pt(t.obs,df=58))
		
		
##############################################
# 6. Performing a t-test in one line of code #
##############################################


	t.test(Head.T,Head.S)
	
	# note the output. The value for the t-statistic should be the same as the one obtained above.  However, the p-value looks a little different, and the degrees of freedom should look completely bizzare. That is because the default setting for the "t.test" function in R performs a slight modification on the degrees of freedom (the Welch-Satterthwaite approximation) that is supposed to account for difference in the variances.   To obtain a result that looks exactly like the one we obtained above:
	t.test(Head.T,Head.S,var.equal=T)
	
	# one-sided
	t.test(Head.T,Head.S,var.equal=T,alternative = c("greater"))
	
	# an important thing to note is that the output of the t.test() function is a "list" which contains named objects which can be accessed in a standard way.  See the "Values" section of the help file "?t.test" to see what the output can be.  
	
	myTest <- t.test(Head.T,Head.S,var.equal=T,alternative = c("greater"))
	myTest$statistic
	myTest$p.value
	myTest$estimate
	
	# all yield test output.
	
	
	
	
	
	
		
		
		
		
		
		
		






