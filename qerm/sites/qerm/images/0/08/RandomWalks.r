# QERM 598
# Lab 6: part A
# Eli Gurarie



####################
# 2-D Random walk  #
####################

# Preface: we need to load this special library for a 'pause' command we'll be using a lot later
	library(DAAG)

# We're going to make some simulations of a random 2-D walker


	T<-1000
	# initial coordinates
	X<-0
	Y<-0
	
	for(i in 1:T)
	{
		# this draws numbers from (-1,0,1)
		dX<-sample(-1:1,1)
		dY<-sample(-1:1,1)
			
		# new position
		X<-c(X,X[i]+dX)
		Y<-c(Y,Y[i]+dY)
		
		# make a plot 
		plot(X,Y,cex=2,pch=20,col=1,type="l",main=i)
		points(X[i],Y[i],pch=20)
	}

	
	# Lets make a function that makes a 2D walker
	RW <- function(r=1,t=100)
	{
		X<-0
		Y<-0
		for(i in 1:T)
		{
			dX<-r*sample(-1:1,1)
			dY<-r*sample(-1:1,1)
			# we're only interested in the last position, so we won't record the tracks
			X<-X+dX
			Y<-Y+dY
		}
		return(c(X,Y))
	}
	
	# Do a bunch of random walks and collect final positions
		Xs<-rep(0,100)
		Ys<-rep(0,100)
		
		for(i in 1:100)
		{
			rw<-RW()
			Xs[i]<-rw[1]
			Ys[i]<-rw[2]
			print(i)
		}
		hist(Xs)
		hist(Ys)
		
	
	# How about two blind Kings on a chess-set?
	# How often will they collide?
	
		Tmax <- 100000
	
		Xmin<- 1
		Ymin<- 1
		Xmax<- 8
		Ymax<- 8
		
		X1<-1
		Y1<-1
		X2<-8
		Y2<-8
		
		a<-matrix(c(0,1),9,9)[1:8,1:8]
		image(1:8,1:8,a,col=c("black","white"),xaxt="n",yaxt="n",xlab="",ylab="")
		axis(1,1:8)
		axis(2,1:8,LETTERS[1:8])
		
		
		
		rm("Ts")
		t<-0
		for(i in 1:Tmax)
		{
			t<-t+1
			dX1<-sample(-1:1,1)
			dY1<-sample(-1:1,1)
			dX2<-sample(-1:1,1)
			dY2<-sample(-1:1,1)
			
			newX1<-max(Xmin,min(Xmax,X1[length(X1)]+dX1))
			newY1<-max(Ymin,min(Ymax,Y1[length(X1)]+dY1))
			newX2<-max(Xmin,min(Xmax,X2[length(X1)]+dX2))
			newY2<-max(Ymin,min(Ymax,Y2[length(X1)]+dY2))
			
			X1<-c(X1,newX1)
			Y1<-c(Y1,newY1)
			X2<-c(X2,newX2)
			Y2<-c(Y2,newY2)

			image(1:8,1:8,a,col=c("white","black"))

			lines(X1,Y1,col=2,lwd=2)
			lines(X2,Y2,col=3,lwd=3)
			
			points(X2,Y2,col=3,type="l")

			if(newX1==newX2&&newY1==newY2)
			{
				points(newX1,newY1,cex=10,pch="*",col=4)
				pause()
				X1<-1
				Y1<-1
				X2<-8
				Y2<-8
				ifelse(exists("Ts"),
					Ts<-c(Ts,t),
					Ts<-t)
				print(Ts)
				t<-0
			}
			else
			{
				points(newX1,newY1,pch=20,cex=3,col=2)
				points(newX2,newY2,pch=20,cex=3,col=3)
			}
			
		}
		
	

	

