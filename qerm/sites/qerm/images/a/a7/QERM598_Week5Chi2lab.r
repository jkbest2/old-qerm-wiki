#####################
# QERM 598			#
# Lab 6: 2.22.2007	#
#####################


# In this lab, we are going to do a chi-squared test on a rxc contingency table
# Load (or enter) the clam data.  These data are small enough that we could just enter it by hand. 

	rm(list=ls())
	
	A<-c(1,49)
	B<-c(15,35)
	C<-c(7,43)
	D<-c(18,32)
	
	Clams.table <- data.frame(A,B,C,D)	
	row.names(Clams.table)<-c("Dead","Alive")
	
# But this is also a good opportunity to get to know the R "table" function.
# Load the Crab data

	Clams.df <- read.csv("Clams.csv",header=T)

# The table function creates contingency tables:
	Clams.table <- table(Clams.df$Survival,Clams.df$Treatment)

# Though this isn't the MOST exciting table in the world, we might as well learn how to save it:
	write.csv(Clams.table, file="ClamsTable.csv", row.names=F)
	
		
	# lets collect some basic objects we'll need later
		R <- rowSums(Clams.table)
		C <- colSums(Clams.table)		
		r <- length(R)
		c <- length(C)
		N <- sum(Clams.table)
		
	
# What's a good way to plot this data?  This isn't so easy to do!
# One option is to make a Bubble plot.  This involves some manipulation!

# We can "undo" the contingency tabulation by using the data.frame command.  
# Note: this only works on "table" objects!

	Clams.table.df <- data.frame(Clams.table)
	names(Clams.table.df) <- c("Status","Treatment","Counts")

	Counts<-Clams.table.df$Counts
	Status <- Clams.table.df$Status
	Treatment <-Clams.table.df$Treatment
	
 
	win.graph(6,4)  					# Make a narrower plotting window
	plot(0,0,type="n",					# Make an empty plot
			main="Bubble Plot of Clam Toxicity Experiment",
			ylim=c(0,3),
			xlim=c(0,5),
			yaxt="n",ylab="Status",		# No y-axis
			xaxt="n",xlab="Toxicant")  	# No x-axis
		
	points(as.integer(Treatment),		# Turn "Treatment" and "Status" into 'integer' 
			as.integer(Status),			# (note this only works for 'factor' objects
			cex=10*sqrt(Counts/max(Counts)), 			# Make size of dots proportional to N
			pch=16,
			col="blue")
	axis(1,								# x axis
		at=1:4,							# tick mark locations
		labels=levels(Treatment))		# level names
		
	axis(2,								# x axis
		at=1:2,							# tick mark locations
		labels=levels(Status))			# level names
	

		
# Anyways, let's do (as always) the chi-squared test by hand				
	# Make the Expected Frequency matrix
		F.hat <- Clams.table*0		
		for(i in 1:r)
		for(j in 1:c)
			F.hat[i,j] <- R[i]*C[j]/N
		
		
	# Calculate Chi statistic
		Chi0 <- sum(((Clams.table-F.hat)/F.hat)^2)
		dof <- (r-1)*(c-1)

	# obtain p-value
		P0 <- 1-pchisq(Chi0,dof)
			
	# And now, of course, for the one-line version
		chisq.test(Clams.table)
	# Do the numbers agree?
	
	
# lets make a generic function that automatically does a chi-squared test
MyChi2Test <- function(mydata,plotme=1)
{
	r <- dim(mydata)[1]
	c <- dim(mydata)[2]
	R <- rowSums(mydata)
	C <- colSums(mydata)	
	N <- sum(mydata)
	
	F.hat <- mydata*0
	for(i in 1:r)
	for(j in 1:c)
		F.hat[i,j] <- R[i]*C[j]/N
	
	dof<-(c-1)*(r-1)	
	Chi0 <- sum((mydata-F.hat)^2/F.hat)		
	P0 <- 1-pchisq(Chi0,dof)

	if(plotme==1)
	{	
		Count <- as.vector(as.matrix(mydata))
		Row <- factor(rep(row.names(mydata),c))
		Column <- factor(rep(names(mydata),each=r))
		plot(0,0,type="n",					
				ylim=c(0,r+1),
				xlim=c(0,c+1),
				yaxt="n",ylab="Columns",	
				xaxt="n",xlab="Rows")  		
		points(as.integer(Column),			
				as.integer(Row),			
				cex=10*sqrt(Count/max(Count)),
				pch=16,
				col="blue")	
		axis(1,								
			at=1:c,							
			labels=levels(Column))			
		axis(2,								
			at=1:r,							
			labels=levels(Row))				
	}		

	return(data.frame(Chi=Chi0,DF=dof,Pvalue=P0))
}
MyChi2Test(Clams.table)

# This might seem a little bit excessive, but when you're actively analyzing 
# many similar datasets, it's often useful to have a function that automatically
# generates the same analysis




# Now lets quickly look at the lizard example:

	SmallD <- c(32,86)
	BigD <- c(11,35)
		
	Anole <- data.frame(SmallD,BigD)
	row.names(Anole) <- c("HighPerch","LowPerch")
	
	MyChi2Test(Anole)
	chisq.test(Anole)
	
	# they're different!   why?
	
	# Because for a 2x2 table R defaults to the Yate's continuity correction".  
	# (for extra virtual credit find out what the Yate's continuity correction is and why it works) 
	# The right (or same) answer is given by:
	
	chisq.test(Anole,correct=FALSE)
	
	# This is one of the reasons it's good to double check 'by-hand-code' what R is automatically doing!
	