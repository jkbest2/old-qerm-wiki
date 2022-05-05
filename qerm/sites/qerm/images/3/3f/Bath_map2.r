#grid data in ASCII raster format with no headers from  http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html
d = read.table('http://wiki.cbr.washington.edu/qerm/images/a/ad/Eli_bath.txt',header=F)
lonvec = seq(130,180,1/15) #4-minute grid
latvec = seq(30,70,1/15)
d2 = d[nrow(d):1,] #why it needs to be flipped over, who knows?

# new and fancier, extending to full range of topo.colors separately for land and water
dmin=min(d2)
dmax=max(d2)
NwaterColors = round(abs(dmin))
NlandColors = round(abs(dmax))

#taking first 1/3 of topo.colors of length NwaterColors for water
waterColorVec = topo.colors(NwaterColors*3)[1:NwaterColors]
#taking second 2/3 of topo.colors of length NlandColors for land
landColorVec = topo.colors(NlandColors*3/2)[round(NlandColors/2+1):round(1.5*NlandColors)]
#combining
allColorVec = c(waterColorVec,landColorVec)

#plot in R
map('worldHires',xlim=c(140,173),ylim=c(43,63), xaxs='i',yaxs='i')
image(lonvec,latvec,t(as.matrix(d2)), col=allColorVec, add=T)
map('worldHires',xlim=c(140,173),ylim=c(43,63), xaxs='i',yaxs='i', add=T)
box()
axis(1, seq(140,180,5),paste(seq(140,180,5),'°E', sep=''))
axis(2, seq(30,70,5),paste(seq(30,70,5),'°N', sep=''))

#same plot exported as PNG file
setwd('/home/ian/Desktop') #set according to your own preference
png('eli_bath2.png',7,7,units='in',res=72) # aspect ratio probably off, could use the maps function maybe
par(mar=rep(2.5,4))
map('worldHires',xlim=c(140,173),ylim=c(43,63), xaxs='i',yaxs='i')
image(lonvec,latvec,t(as.matrix(d2)), col=allColorVec, add=T)
map('worldHires',xlim=c(140,173),ylim=c(43,63), xaxs='i',yaxs='i', add=T)
box()
axis(1, seq(140,180,5),paste(seq(140,180,5),'°E', sep=''))
axis(2, seq(30,70,5),paste(seq(30,70,5),'°N', sep=''))
dev.off()

