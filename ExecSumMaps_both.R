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
if(spp == "PKE") o <- c()
if(spp == "PKO") o <- c()
if(spp == "CM") o <- c()
if(spp == "CO") o <- c()


if(length(o)!=dim(mapDat)[1]) stop("Order not equal to mapDat dim.")

mapDat <- mapDat[o,]
#-----------------------------------------------------------------------------------------
# Map
#-----------------------------------------------------------------------------------------

statusCols <- c(r = "#B66A64", a = "#DFD98D", g = "#8EB687", dd = grey(0.6))

trendPch<-c(r = 25, a = 22, g = 24, dd = 21)

xlims<-c(-127.55, -125.5)
ylims<-c(50.4, 51.15)

# xlims<-extendrange(streamLoc$XLONG) 
# ylims<-extendrange(streamLoc$YLAT)

# Plotting ----------------------------------------------------------------------------------

# Choose metric abund or prod
metric <- "abund"

quartz(width = 5.2, height = 3.3, pointsize = 9)
# pdf(file=paste("map_", spp, ".pdf", sep=""), width = 5.2, height = 3.3, pointsize = 9)
par(mgp=c(1,1,0), family = "Avenir Next")

# Plot base map (empty)
plotMap(nepacLLhigh, xlim=xlims, ylim=ylims, col=NA, border = NA, bg="white", las=1, xlab="", ylab="", xaxt="n", yaxt="n")

# Add blue arrows
polygon(x=A$x, y=A$y, border=NA,col="lightblue")
polygon(x=B$x, y=B$y, border=NA,col="lightblue")
po1,2,7,8,11,12,13,14,16,25,26,35,36,37,38,39,41,42lygon(x=C$x, y=C$y, border=NA,col="lightblue")
polygon(x=D$x, y=D$y, border=NA,col="lightblue")

# Add land over blue arrows
addPolys(nepacLLhigh, col=grey(0.8), border = grey(0.5), bg="white", lwd=0.8)

# Add axes
axis(side=1, at = seq(-128, -125.5, 0.5), cex.axis = 1, labels=sprintf("%.1f", seq(-128, -125.5, 0.5)))
axis(side=2, at = seq(50.4, 51, 0.2), cex.axis = 1, labels=sprintf("%.1f", seq(50.4, 51, 0.2)), las=1)
axis(side=1, at = seq(round(min(xlims), 1), round(max(xlims), 1), 0.1), labels=FALSE, tck = -0.01)
axis(side=2, at = seq(round(min(ylims), 1), round(max(ylims), 1), 0.1), labels=FALSE, tck = -0.01)

# Add streams
if(metric == "prod"){
	points(mapDat$XLONG, mapDat$YLAT, pch=trendPch[mapDat$prod_trendNum], bg = statusCols[mapDat$status_prodNum], lwd=0.5, cex=2.5)
} else if(metric == "abund"){
	points(mapDat$XLONG, jitter(mapDat$YLAT), pch=21, bg = statusCols[mapDat$status_spawnNum], lwd=0.5, cex=2.5)
}
	text(mapDat$XLONG, jitter(mapDat$YLAT), c(1:dim(mapDat)[1]), cex=0.7, font=2)

quartz.save(file=paste("map_", spp, ".pdf", sep=""), type="pdf", width = 5.2, height = 3.3, pointsize = 9)
dev.off()


#-----------------------------------------------------------------------------------------
# Stream names
#-----------------------------------------------------------------------------------------

quartz(width = 2.5, height = 2.8, pointsize = 9)

par(mar=rep(0.1, 4), family="Avenir Next")
plot(1,1,xlim=c(0,1), ylim=c(0,1), "n", bty="n", xaxt="n", yaxt="n")

firstcol <- 1:(ceiling(dim(mapDat)[1]/2))
secondcol <- (max(firstcol)+1):length(o)

points(rep(0.05, length(firstcol)), c(10:(10-length(firstcol)+1))/10, pch=trendPch[mapDat$prod_trendNum[firstcol]], bg = statusCols[mapDat$status_prodNum[firstcol]], lwd=0.5, cex=2.5, xpd=NA)
text(rep(0.05, length(firstcol)), c(10:(10-length(firstcol)+1))/10, firstcol, cex=0.7, font=2)
text(rep(0.12, length(firstcol)), c(10:(10-length(firstcol)+1))/10, mapDat$abbStreamNames[1:length(firstcol)], adj=0)

points(rep(0.55, length(secondcol)), c(10:(10-length(secondcol)+1))/10, pch=trendPch[mapDat$prod_trendNum[secondcol]], bg = statusCols[mapDat$status_prodNum[secondcol]], lwd=0.5, cex=2.5, xpd=NA)
text(rep(0.55, length(secondcol)), c(10:(10-length(secondcol)+1))/10, secondcol, cex=0.7, font=2)
text(rep(0.55+0.07, length(secondcol)), c(10:(10-length(secondcol)+1))/10, mapDat$abbStreamNames[secondcol], adj=0)

quartz.save(file=paste("names_", spp, ".pdf", sep=""), type="pdf", width = 2.5, height = 2.8, pointsize = 9)
dev.off()
