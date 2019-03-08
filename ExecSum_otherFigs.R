library(here)

statusCols <- c(r = "#B66A64", a = "#DFD98D", g = "#8EB687", dd = grey(0.6))
trendPch<-c(r = 25, a = 22, g = 24, dd = 21)

###############################################################################
# Load data and summarize
###############################################################################

dat <- read.csv("1_all_species_status_assessment_table.csv")

#------------------------------------------------------------------------------
# Overall summary across all species

summaryDat <- cbind(
	prod_status = c(g = length(which(dat$status_prod=='green')), a = length(which(dat$status_prod=='amber')), r = length(which(dat$status_prod=='red')), dd = length(which(dat$status_prod=='DD'))),
	prod_trends = c(g = length(which(dat$prod_trend=='green')), a = length(which(dat$prod_trend=='amber')), r = length(which(dat$prod_trend=='red')), dd = length(which(dat$prod_trend=='DD'))),
	sr_status = c(g = length(which(dat$status_SR=='green')), a = length(which(dat$status_SR=='amber')), r = length(which(dat$status_SR=='red')), dd = length(which(dat$status_SR=='DD'))),
	perc_status = c(g = length(which(dat$status_spawn=='green')), a = length(which(dat$status_spawn=='amber')), r = length(which(dat$status_spawn=='red')), dd = length(which(dat$status_spawn=='DD'))),
	perc_status_only = c(g = length(which(dat$status_spawn=='green'&is.na(dat$Smsy)==TRUE)), a = length(which(dat$status_spawn=='amber'&is.na(dat$Smsy)==TRUE)), r = length(which(dat$status_spawn=='red'&is.na(dat$Smsy)==TRUE)), dd = length(which(dat$status_spawn=='DD'&is.na(dat$Smsy)==TRUE)))
	)
summaryDat <- rbind(summaryDat, apply(summaryDat[1:3,], 2, sum), apply(summaryDat, 2, sum))

rownames(summaryDat) <- c("Green", "Amber", "Red", "Data deficient", "Total assessed", "Total attempted")
colnames(summaryDat) <- c("Productivity status", "Productivity trend", "Stock-recruit", "Historic spawners", "Historic spawners only")


#------------------------------------------------------------------------------
# Relationship between productivity status and productivity trend

datProd <- subset(dat, dat$status_prod!="")

prodRel <- matrix(c(
	length(which(datProd$status_prod == "red" & datProd$prod_trend == "red")),
	length(which(datProd$status_prod == "red" & datProd$prod_trend == "amber")),
	length(which(datProd$status_prod == "red" & datProd$prod_trend == "green")),
	length(which(datProd$status_prod == "amber" & datProd$prod_trend == "red")),
	length(which(datProd$status_prod == "amber" & datProd$prod_trend == "amber")),
	length(which(datProd$status_prod == "amber" & datProd$prod_trend == "green")),
	length(which(datProd$status_prod == "green" & datProd$prod_trend == "red")),
	length(which(datProd$status_prod == "green" & datProd$prod_trend == "amber")),
	length(which(datProd$status_prod == "green" & datProd$prod_trend == "green"))
), 3, 3, byrow=FALSE)
rownames(prodRel) <- c("Declining", "Stable/variable", "Increasing")
colnames(prodRel) <- c("Red", "Amber", "Green")

#------------------------------------------------------------------------------
# Summary data by species
nSpp <- length(unique(dat$Species2))
sppSummaryDat <- list(); length(sppSummaryDat) <- nSpp
names(sppSummaryDat) <- unique(dat$Species2)
for(i in 1:nSpp){
	dat.i <- subset(dat, dat$Species2 == unique(dat$Species2)[i])
	sppSummaryDat[[i]] <- cbind(
		prod_status = c(g = length(which(dat.i$status_prod=='green')), a = length(which(dat.i$status_prod=='amber')), r = length(which(dat.i$status_prod=='red')), dd = length(which(dat.i$status_prod=='DD'))),
		prod_trends = c(g = length(which(dat.i$prod_trend=='green')), a = length(which(dat.i$prod_trend=='amber')), r = length(which(dat.i$prod_trend=='red')), dd = length(which(dat.i$prod_trend=='DD'))),
		sr_status = c(g = length(which(dat.i$status_SR=='green')), a = length(which(dat.i$status_SR=='amber')), r = length(which(dat.i$status_SR=='red')), dd = length(which(dat.i$status_SR=='DD'))),
		perc_status = c(g = length(which(dat.i$status_spawn=='green')), a = length(which(dat.i$status_spawn=='amber')), r = length(which(dat.i$status_spawn=='red')), dd = length(which(dat.i$status_spawn=='DD'))),
		perc_status_only = c(g = length(which(dat.i$status_spawn=='green'&is.na(dat.i$Smsy)==TRUE)), a = length(which(dat.i$status_spawn=='amber'&is.na(dat.i$Smsy)==TRUE)), r = length(which(dat.i$status_spawn=='red'&is.na(dat.i$Smsy)==TRUE)), dd = length(which(dat.i$status_spawn=='DD'&is.na(dat.i$Smsy)==TRUE)))
	)
	sppSummaryDat[[i]] <- rbind(sppSummaryDat[[i]], apply(sppSummaryDat[[i]][1:3,], 2, sum), apply(sppSummaryDat[[i]], 2, sum))
	
	rownames(sppSummaryDat[[i]]) <- c("Green", "Amber", "Red", "Data deficient", "Total assessed", "Total attempted")
	colnames(sppSummaryDat[[i]]) <- c("Productivity status", "Productivity trend", "Stock-recruit", "Historic spawners", "Historic spawners only")

} #end species i


###############################################################################
# Plot output
###############################################################################

#------------------------------------------------------------------------------
# Barplots

quartz(width = 2.2, height = 3.6, pointsize = 9)
par(mfrow=c(4,1), mar=c(2,2,1,1), oma=c(2,0,0.5,0), family="Avenir Next", mgp=c(2.5, 0.7, 0))

for(i in c(4,3,1,2)){
	if(i==2|i==4) I <- TRUE else I <- FALSE
	bp <- barplot(summaryDat[3:1,i], col=NA, border=NA, horiz=TRUE,  xlim=c(0, c(rep(30,3), 45)[i]), xaxt="n", ylab = "", names.arg=NA)
	abline(v = seq(0,c(rep(30,3), 45)[i],5), col=grey(0.8))
	if(i!=2) {
		barplot(summaryDat[3:1,i], col=statusCols[c('r', 'a', 'g')], add=TRUE, horiz=TRUE, xaxt="n", ylab = "", names.arg=NA)
		text(summaryDat[3:1,i], bp, col=1, summaryDat[3:1,i], font=2, pos=4)
	} else{
		# segments(x0 = 0, x1 = summaryDat[1:3,i], y0 = bp, y1 = bp, lty=3, col=grey(0.8))
		# abline(h = bp, lty=3, col=grey(0.8))
		points(summaryDat[3:1,i], bp, pch = trendPch[c('r', 'a', 'g')], cex=2.5, bg="white", xpd=NA)
		text(summaryDat[3:1,i]+0.5, bp, summaryDat[3:1,i], font=2, pos=4, xpd=NA)
		text(summaryDat[3:1,i]-0.5, bp, c("declining", "stable/variable", "increasing"), pos=2, xpd=NA, cex=1.2)
		
}
	if(i==4) axis(side=1, at = c(0, 10, 20, 30, 40), labels=I, cex = 1) else axis(side=1, at = c(0, 10, 20, 30), labels=I, cex = 1)
	axis(side=1, at=seq(0, c(rep(30,3), 45)[i], 5), tck=-0.05, labels=FALSE)
	mtext(side = 3, adj=0, LETTERS[c(3,4,2,1)[i]], line=0.3)
if(i==4){
	# mtext(side = 1, "Number of populations", line=2.5)
	# quartz.save(file="barplotA.pdf", type="pdf", width = 2.2, height = 3.6, pointsize = 9)
	# par(mfrow=c(4,1), mar=c(2,2,1,1), oma=c(2,0,0.5,0), family="Avenir Next", mgp=c(2.5, 0.7, 0))
	# plot(1, 1, "n", bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
}
	}
mtext(side = 1, "Number of populations", line=2.5)

# quartz.save(file="barplotB-D.pdf", type="pdf", width = 2.2, height = 3.6, pointsize = 9)
dev.off()

#------------------------------------------------------------------------------
# By species
sppSummaryDat[[7]] <- summaryDat
sppNames <- c("Even-year pink", "Odd-year pink", "Chum", "Coho", "Sockeye", "Chinook", "All")
x0 <- c(1.1, 2.2, 3.4, 4.7)


quartz(width = 7.5, height = 3.5, pointsize = 9)
par(mfrow = c(1, 1), mar=rep(0.5, 4), family = "Avenir Next")

plot(1,1,"n", xlim=c(0.6, 5), ylim=c(0, 10), bty="n", xaxt="n", yaxt="n")
for(i in 1:(nSpp + 1)){
	if(i==7) q <- 0.3 else q <- 0
	# abline(v = 1, lty=3)
	text(1, 8-i-0.5-q, sppNames[i], adj = 1)
	
	for(j in 1:3){
		polygon(x = x0[j] + c(0,1,1,0), y = 8-i-q-c(0.1,0.1,0.9,0.9))
		y <- sppSummaryDat[[i]][1:4,c(4,3,1,2)[j]]
		if(sum(y)==0){
			text(x0[j] + 0.5, 8-i-0.5, "not assessed")
		}else{
			yCum <- c(0, cumsum(y)/sum(y))
			for(s in 1:4){
				polygon(x = x0[j] + yCum[c(s,s+1,s+1,s)], y = 8-i-q-c(0.1,0.1,0.9,0.9), col=statusCols[c('g', 'a', 'r', 'dd')[s]])
				if(y[s]>0) text(x0[j] + mean(yCum[c(s, s+1)]), 8-i-0.5-q, y[s], cex=0.8)
			} #end s
		} #end if assessed
	} #end j
	
	# if(sum(sppSummaryDat[[i]][1:3,2])>0){
	# 	points(c(4.75, 4.9, 5.05), rep(8-i-0.5-q, 3), pch=trendPch[c('g', 'a', 'r')], cex=3, bg="white")
	# 	text(c(4.75, 4.9, 5.05), rep(8-i-0.5-q, 3), cex=0.7, sppSummaryDat[[i]][1:3,2], font=2)
	# }
	# 
} #end s

text(0.9, 7.5, "Species", adj = 1, font=2)
text(2.15, 8.5, "Abundance metric", font = 2)
text(x0[3]+0.5, 8.5, "Productivity metric", font=2)
text(1.6, 7.6, paste("Historic spawners benchmarks\n(n=", summaryDat['Total attempted','Historic spawners'], ")", sep=""))
text(2.7, 7.6, paste("Stock-recruit benchmarks\n(n=", summaryDat['Total attempted','Stock-recruit'], ")"))
text(x0[3]+0.5, 7.6, paste("\n(n=", summaryDat['Total attempted','Productivity trend'], ")"))

quartz.save(file="sumDatSpecies.pdf", type="pdf", width = 7.5, height = 3.5, pointsize = 9)
dev.off()

#------------------------------------------------------------------------------
# Abudnance status vs trend
x <- rep(c(0.5, 1.5, 2.5), 3)
y <- rep(c(0.5, 1.5, 2.5), each = 3)
quartz(width = 3.6, height=3.4, pointsize=9)
par(mfrow = c(1, 1), mar=rep(0.5, 4), family = "Avenir Next")
plot(1,1,"n", xlim=c(-2, 4), ylim=c(0, 9), bty="n", xaxt="n", yaxt="n")

for(i in 1:9){
	points(x[i], y[i], pch = trendPch[rep(c('r', 'a', 'g'), each=3)[i]], bg = statusCols[rep(c('g', 'a', 'r'), 3)[i]], cex=4)
	# if(i==1|i==9) points(x[i], y[i], pch = trendPch[rep(c('r', 'a', 'g'), each=3)[i]], bg="#FFFFFF80", cex=4)
	text(rep(c(0.5, 1.5, 2.5), 3)[i], rep(c(0.5, 1.5, 2.5), each = 3)[i], prodRel[rep(c(1,2,3),each = 3)[i], rep(c(3,2,1), 3)[i]], font=2)
}

text(rep(-0.1, 3), c(0.5, 1.5, 2.5), c("Declining", "Stable/variable", "Increasing"), adj=1)
text(c(0.5, 1.5, 2.5), rep(3.6, 3), c("Green", "Amber", "Red"))
text(1.5, 4.25, "Status", font=2)
text(-0.8, 3.5, "Trend", font=2)
segments(x0=0, x1=0, y0=0, y1=3.2)
segments(x0=0, x1=3, y0=3.2, y1=3.2)

###############################################################################
# Monitoring change
###############################################################################

monDat <- read.csv("monitoring_coverage_table.csv")

max(monDat$year)
monDat[monDat$year==2017,]

sppCols <- colors()[616:619]
sppCols <- colors()[c(616, 619, 621, 624)]
sppCols <- c("#1E3040",  "#C0AF7C", "#D7D7D1", "#736762", "#BDB2B0")

quartz(width = 3.6, height = 3.6, pointsize = 9)
par(family = "Avenir Next", mgp=c(2.5, 0.8, 0), mar=c(3.5, 3.5, 1, 2))
plot(monDat$year[monDat$species=="all"], monDat$no.counted[monDat$species=="all"], "l", las =1, bty="l", xlab = "Year", ylab="Number of systems monitored by DFO", lwd=2, ylim=c(0, 70))
points(2017, 11, pch = 19)
text(2017, 11, "n=11", font = 2, cex=0.8, xpd=NA, pos=4)
for(i in 1:5){
	lines(monDat$year[monDat$species==levels(monDat$species)[i+1]], monDat$no.counted[monDat$species==levels(monDat$species)[i+1]], col = sppCols[i], lwd=0.8)
}

legend(1998, 75, bty="n", lwd=c(2, rep(0.8, 5)), col=c(1, sppCols), legend = c("Any species", levels(monDat$species)[2:6]), cex=1, xpd=NA)

	
