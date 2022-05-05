#Example Four Panel Contour Plot with One Legend
#Author: Carey R McGilliard
#September 2010

#This code uses a modified version of filled.contour called filled.contour3 (created by Carey McGilliard, Ian Taylor, and Bridget Ferris)
 #to make an example figure of four contour plots sharing a legend (to the right).
 #The example demonstrates how to use various color schemes for the contour plots and legend, but the user will want to
 #pick one color scheme for all four plots such that the legend matches the plots.
 #Changing the x- and y-axis values will change the placement of text added to the figure using the text() function and adjustments will be necessary

 
#Source the following functions (change the paths as necessary)
source("./print.letterTrevor.R")
source("./filled.contour3.R")
source("./filled.legend.R")

#gplots has the function colorpanel, which is handy for making gray-scale contour plots
library(gplots)

#------------------------------------------------------
#Generate some fake data
x = rep(c(10,11,12),length = 9)
y = rep(c(1,2,3),each = 3)
z = rnorm(n=9,mean = 0,sd = 1)


    xcoords = unique(x)
    ycoords = unique(y)
    surface.matrix = matrix(z,nrow=length(xcoords),ncol=length(ycoords),byrow=T)
#------------------------------------------------------

#plot.new() is necessary if using the modified versions of filled.contour
plot.new()

#I am organizing where the plots appear on the page using the "plt" argument in "par()"
par(new = "TRUE",              
    plt = c(0.1,0.4,0.60,0.95),   # using plt instead of mfcol (compare
                                  # coordinates in other plots)
    las = 1,                      # orientation of axis labels
    cex.axis = 1,                 # size of axis annotation
    tck = -0.02 )                 # major tick size and direction, < 0 means outside

#Top left plot:
#
# the filled contour - coloured areas
filled.contour3(xcoords,
                ycoords,
                surface.matrix,
                color=terrain.colors,
                xlab = "",        # suppress x-axis annotation
                ylab = "",        # suppress y-axis annotation
                xlim = c(min(xcoords),max(xcoords)),
                ylim = c(min(ycoords),max(ycoords)),
                zlim = c(min(surface.matrix),max(surface.matrix))
                )
# the contour part - draw iso-lines
contour(xcoords,
        ycoords,
        surface.matrix,
        color=terrain.colors,
        xlab = "",
        ylab = "",
        xlim = c(min(xcoords),max(xcoords)),
        ylim = c(min(ycoords),max(ycoords)),
        zlim = c(min(surface.matrix),max(surface.matrix)),
        add=TRUE,                 # add the contour plot to filled-contour,
                                  #thus making an overlay
        col = grey(0.4)           # color of overlay-lines
        )
#
# An annotation inside first plot
#The xpd=NA allows for writing outside the plot limits, but still using the the x and y axes to place the text
par(xpd = NA)
text(x=11,y=1.5,"x",cex = 1.5,font = 2)
print.letter(text = "(a)")

######################################################################
#
#
#Top right plot:
par(new = "TRUE",
    plt = c(0.5,0.8,0.60,0.95),  # defining window for second plot
    las = 1,
    cex.axis = 1)
#
filled.contour3(
                xcoords,
                ycoords,
                surface.matrix,
                color=heat.colors,
                xlab = "",
                ylab = "",
                xlim = c(min(xcoords),max(xcoords)),
                ylim = c(min(ycoords),max(ycoords)),
                zlim = c(-1,1)
                )
#
contour(
       xcoords,
       ycoords,
       surface.matrix,
       xlab = "",
       ylab = "",
       xlim = c(min(xcoords),max(xcoords)),
       ylim = c(min(ycoords),max(ycoords)),
       zlim = c(-1,1),
        add=TRUE
                )
#
#Alternatively, you could set z axis limits to depend
#on the min and max values in surface.matrix.
#filled.contour3(xcoords,ycoords,surface.matrix,color=heat.colors,xlab = "",ylab = "",xlim = c(min(xcoords),max(xcoords)),ylim = c(min(ycoords),max(ycoords)),zlim = c(min(surface.matrix),max(surface.matrix)))
#
# Add annotation
text(x=11,
     y=1.5,
     "x",
     cex = 1.5,
     font = 2)

print.letter(text = "(b)")

######################################################################
#
#Bottom left plot:
par(new = "TRUE",
    plt = c(0.1,0.4,0.15,0.5),
    las = 1,
    cex.axis = 1)
#
filled.contour3(xcoords,
                ycoords,
                surface.matrix,
                col=colorpanel(11, "white", "grey10"),
                nlevels=11,
                xlab = "",
                ylab = "",
                xlim = c(min(xcoords),max(xcoords)),
                ylim = c(min(ycoords),max(ycoords)),
                zlim = c(-1,1))
#
contour(xcoords,
        ycoords,
        surface.matrix,
        xlab = "",
        ylab = "",
        xlim = c(min(xcoords),max(xcoords)),
        ylim = c(min(ycoords),max(ycoords)),
        zlim = c(-1,1),
        add = TRUE)
#
text(x=11,
     y=1.5,
     "x",
     cex = 1.5,
     font = 2,
     col = "white")

print.letter(text = "(c)",printcolor = "blue")

######################################################################
#
#Bottom right plot:
par(new = "TRUE",
    plt = c(0.5,0.8,0.15,0.5),
    las = 1,
    cex.axis = 1)
#
filled.contour3(
                xcoords,
                ycoords,
                surface.matrix,
                color = terrain.colors,
                xlab = "",
                ylab = "",
                xlim = c(min(xcoords),max(xcoords)),
                ylim = c(min(ycoords),max(ycoords)),
                zlim = c(-1,1)
                )
#
contour(
       xcoords,
       ycoords,
       surface.matrix,
       xlab = "",
       ylab = "",
       xlim = c(min(xcoords),max(xcoords)),
       ylim = c(min(ycoords),max(ycoords)),
       zlim = c(-1,1),
        add=TRUE
       )

text(x=11,
     y=1.5,
     "hello",
     cex = 1.5,
     font = 2)
print.letter(text = "(d)")
#
######################################################################
#Add a legend:
par(new = "TRUE",
    plt = c(0.85,0.9,0.25,0.85),   # define plot region for legend
    las = 1,
    cex.axis = 1)
#
filled.legend(
              xcoords,
              ycoords,
              surface.matrix,
              color = terrain.colors,
              xlab = "",
              ylab = "",
              xlim = c(min(xintercepts),max(xintercepts)),
              ylim = c(min(slopes),max(slopes)),
              zlim = c(-1,1))

#Add some figure labels
par(xpd=NA,cex = 1.3)
text(x = -16.7,y = 0,"slope",srt = 90,cex = 1.3)
text(x = -8,y = -1.62,expression(paste(italic(x),"-intercept",sep = "")),cex = 1.3)





