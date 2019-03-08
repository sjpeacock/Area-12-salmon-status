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
spp <- "CK"
#  ***********************************

if(spp == "SO"){
	loc <- subset(loc, loc$SPECIES_QUALIFIED == "SEL"|loc$SPECIES_QUALIFIED == "SER")
}else{
	loc <- subset(loc, loc$SPECIES_QUALIFIED == spp)
}

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
if (spp == "PKE"|spp=="CO")	popNames[popNames == "BOUGHEY CREEK"] <- "BOUGHEY BAY UNNAMED CREEK #2"
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

# Remove "other" Franklin River on WVI
if(spp == "CM" | spp == "CO"){
	streamLoc <- streamLoc[-which(ceiling(streamLoc$XLONG)==-124),]
	}

# For some reason there are duplicate rows for chum and coho. Correct this
if(spp == "CM"|spp == "CO"|spp == "SO"|spp == "CK"){
	for(i in 1:dim(streamLoc)[1]){
		if(length(which(streamLoc$SYSTEM_SITE == streamLoc$SYSTEM_SITE[i])) > 1){
			streamLoc <- streamLoc[-i, ]
		}
	}
}
if(dim(streamLoc)[1] != dim(stat)[1]) stop("Loc has different dim that stat")

# *****************
# CHange BOUGHEY BAY UNNAMED CREEK #2 back to BOUGHEY CREEK
if (spp == "PKE" | spp == "CO"){
	streamLoc$SYSTEM_SITE <- as.character(streamLoc$SYSTEM_SITE)
	streamLoc$SYSTEM_SITE[streamLoc$SYSTEM_SITE=='BOUGHEY BAY UNNAMED CREEK #2'] <- "BOUGHEY CREEK"
	streamLoc$SYSTEM_SITE <- as.factor(streamLoc$SYSTEM_SITE)
}
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
if(spp == "PKE") 
if(spp == "PKO") o <- c(11,2,13,6,7,14,8,3,19,10,20,5,1,17,12,15,4,9,16,18)
if(spp == "CM") o <- c(18,32,2,20,11,13,21,15, 3,4 ,37,36,29,30,10,33,17,38,22,26,7,12,23,35,9,5,1,19,27,14,24,6,16,28,34,31,25)
if(spp == "CO") o <- c(22,12,37,2,24,15,17,25,31,19,3,4,45,44,34,35,14,21,9,46,26,29,5,11,33,7,16,43,13,6,8,1,41,23, 40,30,18,27,10,20,32,42,38,36,28,39)
if(spp == "SO") o<-c(6,3,4,5,8,2,1,7,10,11,9)
if(spp == "CK") o <- c(5,2,3,4,8,1,6,7)
	
if(length(o)!=dim(mapDat)[1]) stop("Order not equal to mapDat dim.")
# mapDat[which(is.element(c(1:dim(mapDat)[1]), o)==FALSE),]

mapDat <- mapDat[o,]

# if(spp == "PKE"){
# 	moveit <- data.frame(
# 		num = c(5,6,7,8,9,10,11,25,26,28,29), 
# 		x = c(-125.9156, -126.0386, -125.8667, -125.9373, -125.9783, -126.0808, -126.1098, -127.1304, -127.2301, -127.5069, -127.5678), 
# 		
# 		y = c(50.74061, 50.73333, 50.62788, 50.79036, 50.85663, 50.85593, 50.92236, 50.56000, 50.59103, 50.67265, 50.72059))
# }
# 
# 
# 
# #*******
# moveit <- data.frame(
# 	+ 		num = c(1,2,7,8,10,11,12,13,15,23,24,30,31,32,33,35,36), 
# 	+ 		x = c(-125.6903, -125.5590, -125.9338, -126.0420, -126.0895, -126.1046, -126.2566, -126.3139, -126.5391, -126.9457, -127.1584, -126.9766, -127.0761, -127.1943, -127.2900, -127.5236, -127.5976), 
# 	+ 		y = c(51.12259, 51.14064, 50.75446, 50.74540, 50.85383, 50.91513, 50.78406, 50.73841, 50.68848, 51.01097, 50.99812, 50.50138, 50.50877, 50.53065, 50.58211, 50.65611, 50.71446))
# 
# #*******
# 
# 
# if(spp == "CO"){
# 	moveit <- data.frame(
# 		num = c(1,2,7,8,11,12,13,14,16,25,26,35:39,41,42), 
# 		x = c(-125.6809, -125.5628, -125.9379, -126.0435, -126.0632, -126.0911, -126.2402, -126.3268, -126.5430, -127.0337, -127.1100, -126.9587, -127.0598, -127.1640, -127.2231, -127.3062, -127.5403, -127.6089), 
# 		y = c(51.12756, 51.12756, 50.74289, 50.74210, 50.87259, 50.91688, 50.78010, 50.75596, 50.68700, 50.84919, 50.89116, 50.47243, 50.47758, 50.49692, 50.53801, 50.58613, 50.65588, 50.72570))
# }

#-----------------------------------------------------------------------------------------
# Map
#-----------------------------------------------------------------------------------------

statusCols <- c(r = "#B66A64", a = "#DFD98D", g = "#8EB687", dd = grey(0.6))

trendPch<-c(r = 25, a = 22, g = 24, dd = 21)

xlims <- c(-128.3, -125.5)
ylims <- c(50.35, 51.2)

# xlims<-extendrange(streamLoc$XLONG) 
# ylims<-extendrange(streamLoc$YLAT)

# Plotting ----------------------------------------------------------------------------------

# Choose metric abund or prod
metric <- "abund"
if(metric == "prod"){
	wasAssessed <- which(mapDat$status_prod!="") 
	pchAll <- trendPch[mapDat$prod_trendNum[wasAssessed]]
	colAll <- statusCols[mapDat$status_prodNum[wasAssessed]]
} else if (metric == "abund"){
		wasAssessed <- which(mapDat$status_spawn!="") 
		pchAll <- rep(21, length(wasAssessed))
		colAll <- statusCols[mapDat$status_spawnNum[wasAssessed]]
}


quartz(width = 5.2, height = 3.3, pointsize = 9)

par(mgp=c(1,1,0), family = "Avenir Next")

# Plot base map (empty)
plotMap(nepacLLhigh, xlim=xlims, ylim=ylims, las=1, xlab="", ylab="", xaxt="n", yaxt="n", col=grey(0.8), border = grey(0.5), bg="white", lwd=0.8)

axis(side=1, at = seq(-128, -125.5, 0.5), cex.axis = 1, labels=sprintf("%.1f", seq(-128, -125.5, 0.5)))
axis(side=2, at = seq(50.4, 51, 0.2), cex.axis = 1, labels=sprintf("%.1f", seq(50.4, 51, 0.2)), las=1)
axis(side=1, at = seq(round(min(xlims), 1), round(max(xlims), 1), 0.1), labels=FALSE, tck = -0.01)
axis(side=2, at = seq(round(min(ylims), 1), round(max(ylims), 1), 0.1), labels=FALSE, tck = -0.01)

# Add streams
if(metric == "prod"|spp=="SO"|spp=="CK"){
	points(mapDat$XLONG[wasAssessed], mapDat$YLAT[wasAssessed], pch=pchAll, bg = colAll, lwd=0.5, cex=2.5)
	text(mapDat$XLONG[wasAssessed], mapDat$YLAT[wasAssessed], wasAssessed, cex=0.7, font=2)
}	else if (metric == "abund"){
	segments(
		x0=mapDat$XLONG[wasAssessed[is.element(wasAssessed, moveit$num)]], 
		y0=mapDat$YLAT[wasAssessed[is.element(wasAssessed, moveit$num)]], 
		x1=moveit$x[is.element(moveit$num, wasAssessed)], 
		y1=moveit$y[is.element(moveit$num, wasAssessed)])
		
		x <- mapDat$XLONG[wasAssessed]; x[is.element(wasAssessed, moveit$num)] <- moveit$x
		y <- mapDat$YLAT[wasAssessed]; y[is.element(wasAssessed, moveit$num)] <- moveit$y
		points(x,y, pch=pchAll, bg = colAll, lwd=0.5, cex=2.5)
		text(x, y, wasAssessed, cex=0.7, font=2)
}

#-----------------------------------------------------------------------------------------
# Stream names
#-----------------------------------------------------------------------------------------
maxN <- 23

firstcol <- 1:(ceiling(length(wasAssessed)/2))
secondcol <- (max(firstcol)+1):length(wasAssessed)

if(metric == "abund") quartz(width = 2.5, height = 4.5, pointsize = 9) else quartz(width = 2.5, height = 5.3, pointsize = 9)

par(mar=rep(0.1, 4), family="Avenir Next")
plot(1,1,xlim=c(0,1), ylim=c(0,1), "n", bty="n", xaxt="n", yaxt="n")

points(rep(0.05, length(firstcol)), c(maxN:(maxN-length(firstcol)+1))/maxN, pch=pchAll[firstcol], bg = colAll[firstcol], lwd=0.5, cex=2.5, xpd=NA)
text(rep(0.05, length(firstcol)), c(maxN:(maxN-length(firstcol)+1))/maxN, wasAssessed[firstcol], cex=0.7, font=2)
text(rep(0.12, length(firstcol)), c(maxN:(maxN-length(firstcol)+1))/maxN, mapDat$abbStreamNames[wasAssessed][1:length(firstcol)], adj=0)

 points(rep(0.55, length(secondcol)), c(maxN:(maxN-length(secondcol)+1))/maxN, pch=pchAll[secondcol], bg = colAll[secondcol], lwd=0.5, cex=2.5, xpd=NA)
text(rep(0.55, length(secondcol)), c(maxN:(maxN-length(secondcol)+1))/maxN, wasAssessed[secondcol], cex=0.7, font=2)
text(rep(0.55+0.07, length(secondcol)), c(maxN:(maxN-length(secondcol)+1))/maxN, mapDat$abbStreamNames[wasAssessed][secondcol], adj=0)

# quartz.save(file=paste("names_", spp, ".pdf", sep=""), type="pdf", width = 2.5, height = 2.8, pointsize = 9)
# dev.off()
