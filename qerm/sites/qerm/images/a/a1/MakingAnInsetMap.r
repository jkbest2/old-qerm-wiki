########################################################
#
# This is sample code for creating a map of a region 
# and displaying a small inset map for orientation
#
# Eli Gurarie
# April 12, 2008
#
########################################################

	# basic mapmaking functions
	library(maps)
	
	# more high-resolution world data (you might not need but what the heck)
	library(mapdata)
	
	# extra stuff like gridlines, etc.
	library(mapproj)
	
	# make basic map
		map("worldHires",
			ylim=c(48,49.5),xlim=c(152.5,155),
			col="thistle",fill=T,
			main="Map of 5 NFS trajectories")
	# draw grid lines
		map.grid(col="grey")
	# since the basic map draws boundaries, I wanted to remove them so I redrew the map with white lines
		map("worldHires",
			ylim=c(48,49.5),xlim=c(152.5,155),
			fill=F,add=T,col="white")
	# add some labels
		text(154.01,48.85,expression(bold("Shiashkotan")))
		text(153.2,48.1,expression(bold("Matua")))
		text(154.51, 49.12015,expression(bold("Kharimkotan")))
		text(154.7372,49.35593,expression(bold("Onnekotan")))

		text(153.2,49.25,expression(italic('OKHOTSK SEA')),col="grey50",cex=2)
		text(154.6,48.7,expression(italic('PACIFIC \n OCEAN')),col="grey50",cex=2)
	# you could make the preceding code more compact by accessing a database of islands names, for example, and their x,y coordinates
		
	# draw a box
		box()
	# OK, that was the easy part.  it's possible it won't at all for you, since the islands are too small even for "worldHires" (you could try world2Hires?)
		
					
	# Next, we create a new graphics space in the lower-right hand corner.  The numbers are proportional distances within the graphics window (xmin,xmax,ymin,ymax) on a scale of 0 to 1.
	# "plt" is the key parameter to adjust
		par(plt = c(0.67, 0.97, 0.03, 0.27), new = TRUE)

	# to make the little map, it's kind of more robust to extract the x-y coordinates of the lines and draw them separately
		world<-map('world',fill=T,
					plot=F,			 # this suppresses plotting
					interior=FALSE)
	# this suppresses the internal boundaries (countries within continents, etc.)
					
	# extract coordinates
		world.x<-world$x			
		world.y<-world$y
		
	# draws a blank box where I want to put the little map, with the x-y limits of the section I want to show of the world
		plot(0,0,type="n",
			xlim=c(130,180),ylim=c(40,70),
			xaxt="n",yaxt="n",xlab="",ylab="",bg="white")
			
	# fill the box with white
		polygon(c(0,360,360,0),c(0,0,90,90),col="white")
		
	# draw the world outlines
		lines(world.x,world.y,col="grey30")
				
	# draw a teeny little box showing where the big map is sampled from
		polygon(x=c(152.5,155,155,152.5,152.5),
				y=c(48,48,49.5,49.5,48),col="white")	
	
	# the next bit is kind of subtle.  I wanted to fill in the box.  The way "world" data works is that each continent/island line is a list of x-y coordinates.  When there is a break, there is a row with two NA's.  Because I want to fill each little bit separately with a color, I have to extract the x-y coordinates for each section and fill it using the polygon command.  This takes a while!  It can be shortened by using a smaller section of the world (using xlim and ylim in the original "world" command), but it will always work... so
	
	# This counts which island/continent group corresponds to each section
			isnas<-cumsum(is.na(world.x))
		
	# This scrolls through these "isnas" and plots a region for each group of xy data.
		for(i in 1:max(isnas))
			polygon(world.x[isnas==i],world.y[isnas==i],col="blanchedalmond")
	
	# some legend action
		text(145,65,expression(italic('RUSSIA')),col="grey20",cex=1.2)
		lines(x=c(152.5,155,155,152.5,152.5),
					y=c(48,48,49.5,49.5,48),
						lty=1,col="darkgreen")

	# finally, draw the final box
		box()
	