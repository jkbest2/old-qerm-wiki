########################################################
#
# This is sample code for creating a map of a region
# and displaying a small inset map for orientation
#
# Eli Gurarie
# edited by Ian Taylor
# April 17, 2008
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

  # I think this is the key command from http://www.stat.auckland.ac.nz/~paul/RGraphics/examples-map.R
    plot.window(xlim=c(130,180),ylim=c(40,70))

  # fill the box with white
    polygon(c(0,360,360,0),c(0,0,90,90),col="white")

  # draw the map
    map(xlim=c(130,180),ylim=c(40,70), interior=FALSE,
          add=TRUE, fill=TRUE, col="blanchedalmond")

  # draw a teeny little box showing where the big map is sampled from
    polygon(x=c(152.5,155,155,152.5,152.5),
        y=c(48,48,49.5,49.5,48),col="white")

  # some legend action
    text(145,65,expression(italic('RUSSIA')),col="grey20",cex=1.2)
    lines(x=c(152.5,155,155,152.5,152.5),
          y=c(48,48,49.5,49.5,48),
            lty=1,col="darkgreen")

  # finally, draw the final box
    box()
