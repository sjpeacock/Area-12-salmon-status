library(PBSmapping)
library(here)
load('areaMap.RData')

#-----------------------------------------------------------------------------------------
# Read in location data
#-----------------------------------------------------------------------------------------


# Limits of map in lat/lon, converted to BCAlbers
lims<-convLLAlb(c(-129, -123),c(49.5, 52))
x1<-seq(-129, -123, 1)
x2<-seq(-129, -123, 0.5)
y1<-seq(50, 52, 1)
y2<-seq(49.5, 52, 0.5)


# Limits of map in lat/lon, converted to BCAlbers
lims<-convLLAlb(c(-129, -123),c(49.8, 51.8))
x1<-seq(-129, -123, 1)
x2<-seq(-129, -123, 0.1)
y1<-seq(50, 51.5, 0.5)
y2<-seq(49.8, 51.8, 0.1)


box0 <- as.PolySet(data.frame(PID = c(1), POS = c(1:4), X = lims[c(1,2,2,1),1], Y = lims[c(1,1,2,2),2]))

# PFMA<-clipPolys(PFMA, xlim=lims[,1], ylim=lims[,2])

# Pop maps:

limsPop<-convLLAlb(c(-128.3, -125.5), c(50.35, 51.2))
box <- as.PolySet(data.frame(PID = c(1), POS = c(1:4), X = limsPop[c(1,2,2,1),1], Y = limsPop[c(1,1,2,2),2]))

# quartz(width = 3.6, height = 2.8, pointsize = 9)
quartz(width = 7.5, height = 3.3, pointsize = 9)
par(family = "Avenir Next")


plotMap(thinPolys(coast_alb, tol=4), col=NA, xlim=lims[,1], ylim=lims[,2], las=1, xlab="", ylab="", border=NA, xaxt="n", yaxt="n", cex.lab=1.3, bty="n")#expression(paste(degree, "Longitude")), expression(paste(degree, "Latitude"))

addPolys(thinPolys(pinkCU1, tol=250), col=grey(0.8), border=grey(0.8))
addLines(rivers_alb, col = grey(0.5), lwd=0.8)
addPolys(lakes_alb, col="white", border=grey(0.5))
addPolys(thinPolys(PFMA[PFMA$PID==12,], tol=250), border="lightblue4", lwd=0.8, col="lightblue")# #D5964E#, col=grey(0.5))
addLines(thinPolys(coast_alb, tol=100), col=grey(0.5), lwd=0.8) 

addPolys(box, lty=2)

# Add axes
axis(side=1, at = convLLAlb(x1, rep(min(y1),length(x1)))[,1], cex.axis = 1, labels=sprintf("%.0f", x1))
axis(side=2, at = convLLAlb(rep(min(x1),length(y1)), y1)[,2], cex.axis = 1, labels=sprintf("%.1f", y1), las=1)
axis(side=1, at = convLLAlb(x2, rep(min(y2),length(x2)))[,1], cex.axis = 1, labels=FALSE, tck=-0.01)
axis(side=2, at = convLLAlb(rep(min(x2),length(y2)), y2)[,2], cex.axis = 1, labels=FALSE, tck=-0.01)
addPolys(box0)

# legend("topright", fill=c("lightblue"),  c("DFO Area 12") , border="lightblue4", bty="n")
# legend("topright", lty = c(NA, 2), lwd=0.8, col=c(NA, 1), c("", "Boundary of status maps"), bty="n")
