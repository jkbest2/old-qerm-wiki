# QERM 598
# Lab 6: part B
# Eli Gurarie

#######################
#    Markov Matrix    #
#######################

	# Remember the essential properties of Markov Matrix: 
	
	#	a)  M must be n x n where n is the number of states
	#	b)  0 <= p_ij <= 1   (probabilities!)
	#	b) 	sum(p_ij) MUST BE = 1.
	
	# Here we do a 5 point lattice with equal probabilities for neighbors
	
	#	A - B
	#	 \ /
	#	| C |
	#	 / \
	#	D - E

	# transition probabilities:
		PA <- c(0,1/3,1/3,1/3,0)
		PB <- c(1/3,0,1/3,0,1/3)
		PC <- c(1/4,1/4,0,1/4,1/4)
		PD <- c(1/3,0,1/3,0,1/3)
		PE <- c(0,1/3,1/3,1/3,0)
	
	# Make Matrix	
		M<-rbind(PA,PB,PC,PD,PE)
	
	# begin process at state A:
		X<-c(1,0,0,0,0)
		

	# plot distribution of states (with pauses)
		for(i in 1:100)
		{
			barplot(X,main=i)
			X<-X%*%M
			pause()
		}

		
	# What if we create an absorbing state?
		PC<-c(0,0,1,0,0)
		M<-rbind(PA,PB,PC,PD,PE)
		X<-c(1,0,0,0,0)
		for(i in 1:100)
		{
			barplot(X,main=i)
			X<-X%*%M     # note the "%" for matrix manipulation
			pause()
		}


	# How to simulate the process?
		# back to non-absorbing center
		PC <- c(1/4,1/4,0,1/4,1/4)
		M<-rbind(PA,PB,PC,PD,PE)
		
		#Identify states
			State<-1:5
			Xs<-rep(0,100)
		# initial state
			X<-1 
			for(i in 1:100)
			{
				X<-sample(State,1,prob=M[X,])  
				# note the use of the 'prob' parameter in Sample
				Xs[i]<-X
			}

	# How to visualize this simulation
		# get coordinates to represent states.
			A<-c(-1,1)
			B<-c(1,1)
			C<-c(0,0)
			D<-c(-1,-1)
			E<-c(1,-1)
			PlotStates<-rbind(A,B,C,D,E)

		# run simulation
			for(i in 1:length(Xs))
			{
				plot(PlotStates,xaxt="n",yaxt="n")
				text(PlotStates*.9,LETTERS[1:5])
				points(x=PlotStates[Xs[i],1],y=PlotStates[Xs[i],2],
							col=2,cex=2,pch=19)
				pause()
			}
		
