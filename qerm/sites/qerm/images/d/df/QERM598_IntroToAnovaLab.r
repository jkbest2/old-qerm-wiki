#####################
# QERM 598			#
# Lab 3 1.23.2007	#
#####################


# We are going to perform the analysis of variance experiment covered in the lecture using R first by hand and later in a single line.

# Enter the data
	x <- c(0,0.5,1.5,2,1,2,2,3,5,5.5,6.5,7)

# specify the corresponding Pie treatment
	Pie <- c(rep("A",4),rep("B",4),rep("C",4))

# All objects in R have a specific 'attribute' - like number, character, date, etc.  In order to do analyses of variances we will be specifying linear models, in which case the attribute of the treatment factors has to be specified as 'factor':
	Pie <- factor(Pie)
	
# the 'factor' attribute has some special features.  For example, you can obtain a vector of the factor levels with:
	levels(Pie)

# make a dataframe:
	Pie.data<-data.frame(x,Pie)
	Pie.data
	names(Pie.data)
# For the purposes of this lab, we won't be using dataframes per se, but it is good to be familiar with them.  Most typically, you won't be entering the data by hand, but reading it from a file, in which case it will be read as a dataframe.  

# Create a boxplot of the data
	par(mfrow=c(2,2))  # breaks up the plotting window into 4 frames
	boxplot(x~Pie)	
# make it more informative
	boxplot(x~Pie,
			main="Pie experiment Boxplot",
			ylab="Zone-out duration (min)",
			xlab="Pie type",
			col=c("green","blue","red"))
# Plot the raw data
	plot(as.integer(Pie),x)
	# can you see a problem with this plot?
	plot(jitter(as.integer(Pie)),x,
			main="Pie experiment data",
			xlab="Pie type",
			ylab="ZOD (min)",
			xaxt="n")  
	axis(1,label=levels(Pie),at=1:3)
# can you guess what the jitter function does and what the axis function does?
	
			
# to obtain group means, we can do something like this:
	A.means<-mean(x[Pie=="A"])
	B.means<-mean(x[Pie=="B"])
	C.means<-mean(x[Pie=="C"])

# but then there's 'tapply', which is a very useful function which applies any function according to factor levels automatically.  Play with it!
	tapply(x,Pie,mean)
	tapply(x,Pie,sd)
	tapply(x,Pie,min)
	tapply(x,Pie,max)
	
# use tapply to save the means and group variances
	x.means <- tapply(x,Pie,mean)
	x.vars<- tapply(x,Pie,var)
	
# Start collecting the pieces of your ANOVA table
	a<-3
	n<-4
	N<-3*4
	
	SS.total <- sum((x-mean(x))^2)
	SS.treatment <- n*sum((x.means-mean(x))^2)

# There are two ways to obtain SS.error; by the definition:
	x.means.vector <- c(rep(x.means[1],4),rep(x.means[2],4),rep(x.means[3],4))
	SS.error <- sum((x-x.means.vector)^2)
	
# or just by taking the difference between the two other SS's 
	SS.error2 <- SS.total - SS.treatment
# Are they equal?


# Now obtain the Mean Square error and treatment...
	MS.error <- SS.error/(N-a)
	MS.treatment <- SS.treatment/(a-1)
	 
# and the F statistic
	F0 <- MS.treatment/MS.error

# assess the F statistic against an F(2,9) distribution
	1-pf(F0,2,9)
# there's your p-value.   


# Of course R can do all that work all at once for you.  In order to do that, you have to specify the model.  Here is where you get introduced to special R-style notation to create linear models:
	Pie.lm <- lm(x~Pie)

# the "~" (tilde) means model x in terms of factor Pie, in other words:  X_ij = mu_i + epsilon_ij.  "Pie.lm" is a 'linear model object' which has a few special features.  Look at:
	Pie.lm
	names(Pie.lm)
	summary(Pie.lm)

# and explore/think about what those outputs might mean.  What are "Pie.lm$fitted.values" and "Pie.lm$residuals".  Have we created any objects by hand that are similar to these?  

# note that in the analysis above we never bothered to check our ANOVA assumptions.  Now is a good time to do just that.  Remember that the epsilon term (which is referred to as the "residuals") must be normal and iid.  

# look at the qqnorm plot:
	qqnorm(Pie.lm$residuals)
	qqline(Pie.lm$residuals)

# look at the 'fitted' versus 'residual' values
	plot(Pie.lm$fit,Pie.lm$res)
	
# are there any trends in the residuals which would indicate that they are not identically distributed?  (recall this is kind of a hokey homemade dataset).   Most of the time, these diagnositic plots are all one needs to verify the assumptions of ANOVA.   R does them automatically with the following command:
	plot(Pie.lm)
	
# Okay, now you're ready for the magic bullet:
	anova(Pie.lm)
# There you have it.  Instant anova.  Does it agree with the results you obtained by hand?

# Now, perform an anova on a shuffling of the data.  Does it fail?







# Just to show off how quick everything really is, here is an ANOVA on some real data.  The following data is a list of Julian dates that female Steller sealions gave birth on a six different rookeries in Russia.  There are ten females per rookery.  

# we could enter the data by hand like this:
	
	BDay <- c(172,171,175,170,167,159,157,170,174,165,171,161,159,160,162,158,167,176,154,159,166,173,189,162,157,182,166,161,173,179,170,156,167,170,167,164,152,167,167,172,167,146,157,168,158,165,193,172,151,163,157,159,185,158,147,164,161,156,176,164)
	Island <- factor(c("Br","Br","Br","Br","Br","Br","Br","Br","Br","Br","K","K","K","K","K","K","K","K","K","K","Lr","Lr","Lr","Lr","Lr","Lr","Lr","Lr","Lr","Lr","M","M","M","M","M","M","M","M","M","M","P","P","P","P","P","P","P","P","P","P","Y","Y","Y","Y","Y","Y","Y","Y","Y","Y"))

# But clearly this is unwieldly (imagine a dataset with a million lines!).  In fact, this is a truncated dataset with only 10 observations per island.  A more complete dataset exists as a text file "SealionBirthData.dat" on the course website.  Download it and put in some directory, say: "c:/data".  Now, you can load it into R using the following command:
	SSLBirth.data <- read.table("./data/SSLbirths.dat",header=T)
	
	
# the very important "header=T" parameter tells R to view the first row of data as the names for the columns.  You are likely to suffer much in the beginning with the "read.table" command because it is finicky about the format of the data and how you call it, but it is an absolutely vital function!
 
	BDay <- SSLBirth.data$BDay
	Island <- SSLBirth.data$Island


# an analysis of variance can take as little as 4 little lines
	boxplot(BDay~Island)
	BDay.lm<-lm(BDay~Island)
	plot(BDay.lm) 
	anova(BDay.lm)
	