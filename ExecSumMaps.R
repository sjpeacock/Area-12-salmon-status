library(PBSmapping)
library(here)

load('map.RData')

#-----------------------------------------------------------------------------------------
# Read in location data
#-----------------------------------------------------------------------------------------

# DFO location data ------------------------------------------
loc <- read.csv("Conservation_Unit_System_Sites.csv")

# ****** Choose species **************
# unique(loc$SPECIES_QUALIFIED)
spp <- "PKE"
#  ***********************************

loc <- subset(loc, loc$SPECIES_QUALIFIED == spp)

# Emma's status assessment -----------------------------------
stat <- read.csv("1_all_species_status_assessment_table.csv")
if(spp == "PKE" | spp == "PKO"){
	stat <- subset(stat, stat$Species=="PK")
} else {
	stat <- subset(stat, stat$Species == spp)
}

# # Focusing on productivity metrics, so remove populations with no SR model
# stat <- subset(stat, stat$status_prod!="")
# Now plotting both abudance (by historic spawners) AND productivity

# Remove "E" and "O" from river names and subset to only include relevant populations
if(spp == "PKE" | spp == "PKO"){
	if(spp == "PKE") spp2 <- "E" else spp2 <- "O"
	keep <- c(NA) # which Population names are E or O?
	pop <- strsplit(as.character(stat$Population), split = " ")
	pop2 <- c()
	for(i in 1:length(pop)){
		if(tail(pop[[i]], 1) == spp2){
			keep <- c(keep, i)
			popi <- pop[[i]][1]
			for(j in 2:(length(pop[[i]])-1)){
				popi <- paste(popi, pop[[i]][j], sep=" ")
			} # end j
			pop2 <- append(pop2, popi)
		} # end if
	} # end i
	popNames <- pop2
	stat <- stat[keep[2:length(keep)],]
	stat$Population <- popNames # Remove E and O  in stat dataset
} else {
	popNames <- as.character(stat$Population)
}

# *****************
# PKE has one creek by a different name in NuSEDS:
if (spp == "PKE")	popNames[popNames == "BOUGHEY CREEK"] <- "BOUGHEY BAY UNNAMED CREEK #2"
# *****************


#-----------------------------
# Check: are all sites in Emma's data in nuSEDS data?
if(sum(is.element(popNames, loc$SYSTEM_SITE)) != length(popNames)) stop("Some site(s) not in nuSEDS location data") else cat("All sites in nuSEDS location data")

# # When this throws an error, which site is it?
# for(i in 1:length(popNames)){
# 	I <- which(loc$SYSTEM_SITE == popNames[i])
# 	if(length(I) == 0) stop(paste(popNames[i], "not in NuSEDS"))
# }

#-------------------------------

streamLoc <- loc[is.element(loc$SYSTEM_SITE, popNames), c('SYSTEM_SITE', 'XLONG', 'YLAT')]

# For some reason there are duplicate rows for chum and coho. Correct this
if(spp == "CM"|spp == "CO"){
	for(i in 1:dim(streamLoc)[1]){
		if(length(which(streamLoc$SYSTEM_SITE == streamLoc$SYSTEM_SITE[i])) > 1){
			streamLoc <- streamLoc[-i, ]
		}
	}
}
if(dim(streamLoc)[1] != dim(stat)[1]) stop("Loc has different dim that stat")

# *****************
# CHange BOUGHEY BAY UNNAMED CREEK #2 back to BOUGHEY CREEK
streamLoc$SYSTEM_SITE <- as.character(streamLoc$SYSTEM_SITE)
streamLoc$SYSTEM_SITE[streamLoc$SYSTEM_SITE=='BOUGHEY BAY UNNAMED CREEK #2'] <- "BOUGHEY CREEK"
streamLoc$SYSTEM_SITE <- as.factor(streamLoc$SYSTEM_SITE)
# *****************


# Strip out capitals to get main name
streamNames <- sapply(as.character(streamLoc$SYSTEM_SITE), function(v) {
	if (is.character(v)){
		dum1 <- strsplit(as.character(v), split=" ")
		v.dum <- character(length(dum1[[1]]))
		v.new <- character(1)
		for(i in 1:length(dum1[[1]])){
			dum2 <- strsplit(dum1[[1]][i], split = NULL)
			v.dum[i] <- dum2[[1]][1]
			for(j in 2:length(dum2[[1]])){
				v.dum[i] <- paste(v.dum[i], tolower(dum2[[1]][j]), sep = "")
			}
			if(i==1) v.new <- paste(v.new, v.dum[i], sep="") else v.new <- paste(v.new, v.dum[i], sep=" ") 
		}
		
		return(v.new)
	} else {
		return(v)
	}
})

# Strip out RIver and Creek for displayed name
abbStreamNames <- streamNames
for(i in 1:length(streamNames)){
	dum <- strsplit(streamNames[i], split = " ")
	abbStreamNames[i] <- dum[[1]][1]
	if(length(dum[[1]]) > 2){
		for(j in 2:(length(dum[[1]])-1)) abbStreamNames[i] <- paste(abbStreamNames[i] , dum[[1]][j])
	}
}


# PHEW!

# Put together in order to make sure things match!
mapDat <- cbind(abbStreamNames = abbStreamNames[order(streamLoc$SYSTEM_SITE)], streamLoc[order(streamLoc$SYSTEM_SITE),], stat[,c('Population', "status_prod", "prod_trend", "status_spawn")])

# 3 = green, 2 = amber, 1 = red, 0 = data deficient in Emma's data
mapDat$status_prodNum <- as.numeric(factor(mapDat$status_prod, levels=c("red", "amber", "green", "DD", "")))
mapDat$status_spawnNum <- as.numeric(factor(mapDat$status_spawn, levels=c("red", "amber", "green", "DD", "")))
mapDat$prod_trendNum <- as.numeric(factor(mapDat$prod_trend, levels=c("red", "amber", "green", "DD", "")))



# Last thing: order points in pleasing arrangement
# o <- order(mapDat$XLONG)
# o <- c(1:length(mapDat$XLONG))

# Order for productivity maps only:
# if(spp == "PKO") o <- c(2,5,6,10,7,3,12,9,13,1,4,8,11)#c(11,2,13,6,7,14,8,3,18,10,19,5,1,12,15,4,9,16,17)
# if(spp == "PKE") o <- c(12,2,14,6,7,15,8,3,11,20,21,5,1,9,13,17,4,10,18,19,16)
# if(spp == "CM") o <- c(1,3,4,2,11,10,5,12,6,7,8,9)
# if(spp == "CO") o <- c(2,8,5,3,10,9,7,11,1,4,6)

# Order for abundance maps :
if(spp == "PKE")	o <- c(16,2,18,10, 11, 19, 23,20,12 ,3, 4, 32, 26,9,15, 33,8,6,5,1,30 ,13, 17,24,21,7,14,25,31,28,27,22,29)


if(spp == "PKO") o <- c()
if(spp == "CM") o <- c()
if(spp == "CO") o <- c()


if(length(o)!=dim(mapDat)[1]) stop("Order not equal to mapDat dim.")

mapDat <- mapDat[o,]

if(spp == "PKE"){
	moveit <- data.frame(
		num = c(5,6,7,8,9,10,11,25,26,28,29), 
		x = c(-125.9156, -126.0386, -125.8667, -125.9373, -125.9783, -126.0808, -126.1098, -127.1304, -127.2301, -127.5069, -127.5678), 
		
		y = c(50.74061, 50.73333, 50.62788, 50.79036, 50.85663, 50.85593, 50.92236, 50.56000, 50.59103, 50.67265, 50.72059))
}

mapDat$PopNum <- c(1:dim(mapDat)[1])
#-----------------------------------------------------------------------------------------
# Map
#-----------------------------------------------------------------------------------------

statusCols <- c(r = "#B66A64", a = "#DFD98D", g = "#8EB687", dd = grey(0.6))

trendPch<-c(r = 25, a = 22, g = 24, dd = 21)

# xlims<-c(-127.55, -125.5)
# ylims<-c(50.4, 51.15)

xlims <- c(-128.3, -125.5)
ylims <- c(50.35, 51.2)
xlims<-extendrange(streamLoc$XLONG)
ylims<-extendrange(streamLoc$YLAT)

# Blue arrow polygons
A$x[c(13,14,16,17)] = -126.87
A$x[15] = -126.92
A$y[13:17] = c(50.71,50.68,50.725,50.77,50.74)
A$x[13:17] = c(-126.845,-126.825,-126.895,-126.87,-126.87) +0.03
A$y[13:17] = c(50.71,50.7,50.7125,50.755,50.74) +0.01

# Adding extra polygons for shaded regions #
B <- data.frame(x=c(-125.5,-125.85,-125.85,-125.5,-125.5),
								y=c(51.1,50.75,50.6,50.7,51.1))
C <- data.frame(x=c(-126.2,-126.4,-126.4,-126.55,-126.55,-126.75,-126.73,-126.80,-126.73,-126.75,-126.2,-126.2),
								y=c(50.66,50.66,50.7,50.7,50.66,50.64,50.66,50.63,50.6,50.62,50.62,50.66))
D <- data.frame(x=c(-126,-126.65,-126.65,-126,-126),
								y=c(51.05,51.05,50.8,50.8,51.05))  

# Plotting ----------------------------------------------------------------------------------

# Choose metric abund or prod
metric <- "prod"
if(metric == "prod") hasProd <- which(mapDat$status_prod!="")

quartz(width = 5.2, height = 3.3, pointsize = 9)
# pdf(file=paste("map_", spp, ".pdf", sep=""), width = 5.2, height = 3.3, pointsize = 9)
par(mgp=c(1,1,0), family = "Avenir Next")

# Plot base map (empty)
plotMap(nepacLLhigh, xlim=xlims, ylim=ylims, las=1, xlab="", ylab="", xaxt="n", yaxt="n", col=grey(0.8), border = grey(0.5), bg="white", lwd=0.8)

# Add axes
axis(side=1, at = seq(-128, -125.5, 0.5), cex.axis = 1, labels=sprintf("%.1f", seq(-128, -125.5, 0.5)))
axis(side=2, at = seq(50.4, 51, 0.2), cex.axis = 1, labels=sprintf("%.1f", seq(50.4, 51, 0.2)), las=1)
axis(side=1, at = seq(round(min(xlims), 1), round(max(xlims), 1), 0.1), labels=FALSE, tck = -0.01)
axis(side=2, at = seq(round(min(ylims), 1), round(max(ylims), 1), 0.1), labels=FALSE, tck = -0.01)

# Add streams
if(metric == "prod"){
	points(mapDat$XLONG, mapDat$YLAT, pch=trendPch[mapDat$prod_trendNum], bg = statusCols[mapDat$status_prodNum], lwd=0.5, cex=2.5)
	text(mapDat$XLONG[mapDat$prod_trend!=""], mapDat$YLAT[mapDat$prod_trend!=""], c(1:dim(mapDat)[1])[mapDat$prod_trend!=""], cex=0.7, font=2)
} else if(metric == "abund"){
	if(spp == "PKE"){
		segments(x0=mapDat$XLONG[is.element(c(1:dim(mapDat)[1]), moveit$num)], y0=mapDat$YLAT[is.element(c(1:dim(mapDat)[1]), moveit$num)], x1=moveit$x, y1=moveit$y)
		x <- mapDat$XLONG; x[is.element(c(1:dim(mapDat)[1]), moveit$num)] <- moveit$x
		y <- mapDat$YLAT; y[is.element(c(1:dim(mapDat)[1]), moveit$num)] <- moveit$y
		points(x,y, pch=21, bg = statusCols[mapDat$status_spawnNum], lwd=0.5, cex=2.5)
		text(x, y, c(1:dim(mapDat)[1]), cex=0.7, font=2)
	} else {
		text(mapDat$XLONG, mapDat$YLAT, c(1:dim(mapDat)[1]), cex=0.7, font=2)
		
	}
}

# k <- 21:25
# text(mapDat$XLONG[k], mapDat$YLAT[k], c(k), cex=0.7, font=2)

quartz.save(file=paste("map_", spp, ".pdf", sep=""), type="pdf", width = 5.2, height = 3.3, pointsize = 9)
dev.off()


#-----------------------------------------------------------------------------------------
# Stream names
#-----------------------------------------------------------------------------------------

maxN <- 23

if(metric == "prod"){
	firstcol <- 1:(ceiling(length(hasProd)/2))
	secondcol <- (max(firstcol)+1):length(hasProd)
	pchAll <- trendPch[mapDat$prod_trendNum[hasProd]]
	colAll <- statusCols[mapDat$status_prodNum[hasProd]]
	names <- mapDat$abbStreamNames[hasProd]
} else if(metric == "abund") {
	firstcol <- 1:(ceiling(dim(mapDat)[1]/2))
	secondcol <- (max(firstcol)+1):length(o)
	pchAll <- rep(21, dim(mapDat)[1])
	colAll <- statusCols[mapDat$status_spawnNum]
	names <- mapDat$abbStreamNames
}


# quartz(width = 2.5, height = 2.8, pointsize = 9)
quartz(width = 2.5, height = 4.5, pointsize = 9)

par(mar=rep(0.1, 4), family="Avenir Next")
plot(1,1,xlim=c(0,1), ylim=c(0,1), "n", bty="n", xaxt="n", yaxt="n")

points(rep(0.05, length(firstcol)), c(maxN:(maxN-length(firstcol)+1))/maxN, pch=pchAll[firstcol], bg = colAll[firstcol], lwd=0.5, cex=2.5, xpd=NA)
text(rep(0.05, length(firstcol)), c(maxN:(maxN-length(firstcol)+1))/maxN, firstcol, cex=0.7, font=2)
text(rep(0.12, length(firstcol)), c(maxN:(maxN-length(firstcol)+1))/maxN, names[1:length(firstcol)], adj=0)

points(rep(0.55, length(secondcol)), c(maxN:(maxN-length(secondcol)+1))/maxN, pch=pchAll[secondcol], bg = colAll[secondcol], lwd=0.5, cex=2.5, xpd=NA)
text(rep(0.55, length(secondcol)), c(maxN:(maxN-length(secondcol)+1))/maxN, secondcol, cex=0.7, font=2)
text(rep(0.55+0.07, length(secondcol)), c(maxN:(maxN-length(secondcol)+1))/maxN, names[secondcol], adj=0)

quartz.save(file=paste("names_", spp, ".pdf", sep=""), type="pdf", width = 2.5, height = 2.8, pointsize = 9)
dev.off()
