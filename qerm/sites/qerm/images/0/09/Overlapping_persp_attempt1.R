# making overlapping surfaces in R using persp
# by Ian Taylor for James Murphy, because he deserves it.
# started Feb 19, 2009

# setup some basic stuff
x = seq(0,.2,length=30)
y = seq(0,20,length=25)

# create 3 surfaces pass through each other overlap
z1 = matrix(seq(0.1,14,length=30),30,25)
z2 = matrix(seq(4,6.2,length=30),30,25)
z3 = matrix(seq(0,8,length=30),30,25)

# calculate the order of the surfaces
zorder = array(NA,dim=c(30,25,3))
for(irow in 1:nrow(zorder))
{
  for(icol in 1:ncol(zorder))
  {
    zorder[irow,icol, ] <- order(c(z1[irow,icol],z2[irow,icol],z3[irow,icol]))
  }
}

# subset each one based on the order
z1a = z1
z1b = z1
z1c = z1
z1a[zorder[,,1]!=1]=NA
z1b[zorder[,,2]!=1]=NA
z1c[zorder[,,3]!=1]=NA

z2a = z2
z2b = z2
z2c = z2
z2a[zorder[,,1]!=2]=NA
z2b[zorder[,,2]!=2]=NA
z2c[zorder[,,3]!=2]=NA

z3a = z3
z3b = z3
z3c = z3
z3a[zorder[,,1]!=3]=NA
z3b[zorder[,,2]!=3]=NA
z3c[zorder[,,3]!=3]=NA

# plot bottom layer
persp(x,y,z1a,phi=25,theta=-45,ltheta=120,lphi=0,col=rgb(1,0,0,.5),zlim=c(0,14),zlab='z',ticktype='detailed')
par(new=T)
persp(x,y,z2a,phi=25,theta=-45,ltheta=120,lphi=0,col=rgb(0,1,0,.5),zlim=c(0,14),axes=F)
par(new=T)
persp(x,y,z3a,phi=25,theta=-45,ltheta=120,lphi=0,col=rgb(0,0,1,.5),zlim=c(0,14),axes=F)
par(new=T)

# plot middle layer
persp(x,y,z1b,phi=25,theta=-45,ltheta=120,lphi=0,col=rgb(1,0,0,.5),zlim=c(0,14),axes=F)
par(new=T)
persp(x,y,z2b,phi=25,theta=-45,ltheta=120,lphi=0,col=rgb(0,1,0,.5),zlim=c(0,14),axes=F)
par(new=T)
persp(x,y,z3b,phi=25,theta=-45,ltheta=120,lphi=0,col=rgb(0,0,1,.5),zlim=c(0,14),axes=F)
par(new=T)

# plot top layer
persp(x,y,z1c,phi=25,theta=-45,ltheta=120,lphi=0,col=rgb(1,0,0,.5),zlim=c(0,14),axes=F)
par(new=T)
persp(x,y,z2c,phi=25,theta=-45,ltheta=120,lphi=0,col=rgb(0,1,0,.5),zlim=c(0,14),axes=F)
par(new=T)
persp(x,y,z3c,phi=25,theta=-45,ltheta=120,lphi=0,col=rgb(0,0,1,.5),zlim=c(0,14),axes=F)
par(new=T)

