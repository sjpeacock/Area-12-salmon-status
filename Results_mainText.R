###############################################################################
# Colourful results plots 
# Aug 10, 2020
###############################################################################

statusCols <- c(g = "#8EB687", a = "#DFD98D", r = "#9A3F3F")##B66A64")
font2use <- "Helvetica"#Avenir Next"

library('gsl') # for LambertW0 function to calculate Smsy
library('gplots')
library(rjags)

source("Results_functions.R")

statusDat <- read.csv("Output/1_all_species_status_assessment_table_Aug-2019.csv")

###############################################################################
# Status tables
###############################################################################

for(s in 1:6){ # for each species
	
	#-----------------------------------------------------------------------------
	# Subset statusDat for that species
	#-----------------------------------------------------------------------------
	sp <- c("PKE", "PKO", "CM", "CK", "SO", "CO")[s]
	statusDat1 <- subset(statusDat, statusDat$Spp2 == sp)
	nR <- dim(statusDat1)[1]
	
	# Order statusDat alphabeically by River name
	# ** Note that I combined rivers that were and were not assesed by SR benchmarks,
	# because I thought this made it easier for a reader to locate a population they
	# may be interested in.
	statusDat1 <- statusDat1[order(statusDat1$Population), ]
	
	#-----------------------------------------------------------------------------
	# Create pretty names (Lower case) for the population
	#-----------------------------------------------------------------------------
	# Remove "E" and "O" from river names and subset to only include relevant populations
	if(sp == "PKE" | sp == "PKO"){
		if(sp == "PKE") sp2 <- "E" else sp2 <- "O"
		keep <- c(NA) # which Population names are E or O?
		pop <- strsplit(as.character(statusDat1$Population), split = " ")
		pop2 <- c()
		for(i in 1:length(pop)){
			if(tail(pop[[i]], 1) == sp2){
				keep <- c(keep, i)
				popi <- pop[[i]][1]
				for(j in 2:(length(pop[[i]])-1)){
					popi <- paste(popi, pop[[i]][j], sep=" ")
				} # end j
				pop2 <- append(pop2, popi)
			} # end if
		} # end i
		popNames <- pop2
		statusDat1 <- statusDat1[keep[2:length(keep)],]
		statusDat1$Population <- popNames # Remove E and O  in stat dataset
	} else {
		popNames <- as.character(statusDat1$Population)
	}
	# Split and make lower case
	streamNames <- sapply(as.character(statusDat1$Population), function(v) {
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
	
	# Remove N* Creek
	streamNames[streamNames == "Nigger Creek"] <- "N* Creek"
	
	# Add footnote for Klinaklini
	streamNames[streamNames == "Klinaklini River"] <- expression(paste("Klinaklini River", {}^2, sep = ""))
	
	#-----------------------------------------------------------------------------
	# Plot
	#-----------------------------------------------------------------------------
	# quartz(width = 8, height = nR/6+0.2, pointsize = 10)
	if(sp == "CK"){
		pdf(file = paste("Results/Table_", sp, ".pdf", sep = ""), width = 8, height = 2.033333, pointsize = 10)
		par(mar = c(1,1,1.5,10), family = font2use)
		plot(seq(1, 6, length.out = 12), -2:(nR+1), "n", bty ="n", xaxt="n", yaxt="n", xlab = "", ylab = "")
		
	} else{
		
		pdf(file = paste("Results/Table_", sp, ".pdf", sep = ""), width = 8, height = nR/6+0.2, pointsize = 10)
		par(mar = c(1,1,1.5,10), family = font2use)
		plot(seq(1, 6, length.out = nR + 1), 1:(nR+1), "n", bty ="n", xaxt="n", yaxt="n", xlab = "", ylab = "")
	} 
	
	
	
	text(rep(2, nR), nR:1, streamNames, pos = 2, xpd = NA,cex = 0.8)
	text(c(2.5, 3.5, 4.5, 5.5), rep(nR+1.8, 4), c("Abundance\n(percentile)", "Abundance\n(spawner-recruit)", "Resilience", "Resilience\ntrend"), xpd =NA, font = 2)
	
	# Plot coloured status polygons for each river
	for(i in 1:nR){
		
		for(j in 1:3){ #for each metric/benchmark combo
			status.ij <- statusDat1[i, c('status_spawn', "status_SR", "status_prod")[j]]
			density.ij <- NULL
			if(is.na(status.ij) == TRUE){
				col.ij <- "white"
			} else if(status.ij == 4){
				col.ij <- grey(0.8)
				density.ij <- 40
				} else {
					col.ij <- statusCols[c('r', 'a', 'g')[status.ij]]
				}
			
			polygon(x = c(1.1, 1.9, 1.9, 1.1) + j, y = (nR - i + 1) + rep(c(0.45, -0.45), each = 2), col = col.ij, border = NA, density = density.ij)
			
		} # end j
		
		# Add the text for productivity trend
		if(is.na(statusDat1$prod_trend[i]) == FALSE){
			text(5.5,  (nR - i + 1), c("Decreasing", "Stable/Variable", "Increasing", "DD") [statusDat1$prod_trend[i]], cex = 0.8, col = c(rep(1, 3), grey(0.8))[statusDat1$prod_trend[i]])
		}
	} #end i
	
	# Horizontal lines to make it easier to read 
	abline(h = seq(1, nR+1, 3)-0.475, lwd = 0.8)
	
	# Add legend
	text(6.75, nR+1.8, "Status\nlegend", xpd =NA, font = 2)
	
	for(j in 1:5){
		if(j==4){
			polygon(x = c(6.4, 7.1, 7.1, 6.4), y = (nR - j*2 + 1) + rep(c(0.9, -0.9), each = 2), xpd = NA, col = grey(0.8), density = 40, border = NA)
			} else {
				polygon(x = c(6.4, 7.1, 7.1, 6.4), y = (nR - j*2 + 1) + rep(c(0.9, -0.9), each = 2), col = c(statusCols[c('g', 'a', 'r')], grey(0.8), "white")[j], border = c(rep(NA, 4), grey(0.8))[j], lty = 3, xpd = NA)
			}
	text(6.75, mean((nR - j*2 + 1) + rep(c(0.9, -0.9), each = 2)), c("green", "amber", "red", expression(paste("data deficient", {}^1, sep="")), expression(paste("not assessed", {}^1, sep="")))[j], xpd = NA, col = c(1, 1, "white", 1)[j], cex = 0.8)
	}
	
	dev.off()
} # end spp s

###############################################################################
# Spawners/productivity figures
###############################################################################

#------------------------------------------------------------------------------
# Fig. 2: Embley River Even-year pink
#------------------------------------------------------------------------------
load("Output/7b_pink_model-fitting and plots_environment_NEW.RData")

# Even year
p.name <- "EMBLEY CREEK E"
sp <- "PKE"
gen <- 1 # Generation length (spread in age-at-return)
source("selectPopulation.R")

quartz(width = 6.8, height = 2.6, pointsize = 10)
par(mfrow = c(1,2), mar = c(3,5,2,1), family = font2use)

# For this plot, don't include benchmarks or status colours. It is intended
# to illsutrate the data and metrics rather than status.
# Note because it's pink (1-year generation length), the metric (generational
# average) is the same as the year's spawner abundance.

plot.currentS(year = years, spawners = dat.p$Spawners, currentS = currentS, BMs = matrix(NA, 34, 2), status = rep(1, 34), incl.legend = FALSE, labRight = c(expression(italic(S)[GEN1]), expression(italic(S)[MSY])))
mtext(side = 3, adj = 0, line = 0.5, "A")

plot.prod(year = years, a = a, a.year = a.year, status = rep("white", 34))
polygon(x = c(years[a.year], rev(years[a.year])), y = exp(c(a[,2], rev(a[,3]))), col = grey(0.8), border = NA)
lines(years[a.year], exp(a[,1]))
abline(h = 1, lty = 2)
mtext(side = 3, adj = 0, line = 0.5, "B")


#------------------------------------------------------------------------------
# Fig. 5: Ahta even and odd pink
#------------------------------------------------------------------------------
load("Output/7b_pink_model-fitting and plots_environment_NEW.RData")

quartz(width = 7, height = 5, pointsize = 10)
par(mfrow = c(2,2), mar = c(3,5,2,1), family = font2use, oma = c(0, 3, 0, 0))

# Even year
p.name <- "AHTA RIVER E"
sp <- "PKE"
p <- which(use.rivers == p.name)
gen <- 1# Generation length (spread in age-at-return)
source("selectPopulation.R")

plot.currentS(year = years, spawners = dat.p$Spawners, currentS = currentS, BMs = annualBMs$SR, status = status$SR, incl.legend = FALSE, labRight = c(expression(italic(S)[GEN1]), expression(italic(S)[MSY])))
# Can plot percentile benchamrks instead using the following:
# plot.currentS(year = years, spawners = dat.p$Spawners, currentS = currentS, BMs = annualBMs$percentile, status = status$percentile, incl.legend = FALSE)
mtext(side = 3, adj = 0, line = 0.5, "A")
mtext(side = 2, line = 5, "Ahta River Even")

plot.prod(year = years, a = a, a.year = a.year, status = status$resilience)
mtext(side = 3, adj = 0, line = 0.5, "B")

# Odd year
p.name <- "AHTA RIVER O"
sp <- "PKO"
p <- which(use.rivers == p.name)
gen <- 1# Generation length (spread in age-at-return)
source("selectPopulation.R")

plot.currentS(year = years, spawners = dat.p$Spawners, currentS = currentS, BMs = annualBMs$SR, status = status$SR, incl.legend = FALSE, labRight = c(expression(italic(S)[GEN1]), expression(italic(S)[MSY])))
mtext(side = 3, adj = 0, line = 0.5, "C")
mtext(side = 2, line = 5, "Ahta River Odd")

plot.prod(year = years, a = a, a.year = a.year, status = status$resilience)
mtext(side = 3, adj = 0, line = 0.5, "D")


#------------------------------------------------------------------------------
# Fig. 6: Viner Sound Chum 
#------------------------------------------------------------------------------
load("Output/8b_chum_model-fitting and plots_environment_NEW.RData")
p <- 11
p.name <- unique(dat$River)[p]
sp <- "CM"
gen <- 4# Generation length (spread in age-at-return)
source("selectPopulation.R")

quartz(width = 6.8, height = 2.6, pointsize = 10)
par(mfrow = c(1,2), mar = c(3,5,2,1), family = font2use)

# Can plot percentile benchamrks instead using the following:
# plot.currentS(year = years, spawners = dat.p$Spawners, currentS = currentS, BMs = annualBMs$percentile, status = status$percentile, incl.legend = FALSE)
plot.currentS(year = years, spawners = dat.p$Spawners, currentS = currentS, BMs = annualBMs$SR, status = status$SR, incl.legend = FALSE, labRight = c(expression(italic(S)[GEN1]), expression(italic(S)[MSY])))
mtext(side = 3, adj = 0, line = 0.5, "A")

plot.prod(year = years, a = a, a.year = a.year, status = status$resilience)
mtext(side = 3, adj = 0, line = 0.5, "B")

#------------------------------------------------------------------------------
# Fig. 7: Wakeman coho
#------------------------------------------------------------------------------
load("Output/9b_coho_model-fitting and plots_environment_NEW.RData")
p.name <- "WAKEMAN RIVER"
sp <- "CO"
gen <- 4# Generation length (spread in age-at-return)
source("selectPopulation.R")

quartz(width = 6.8, height = 2.6, pointsize = 10)
par(mfrow = c(1,2), mar = c(3,5,2,1), family = font2use)

# Can plot percentile benchamrks instead using the following:
# plot.currentS(year = years, spawners = dat.p$Spawners, currentS = currentS, BMs = annualBMs$percentile, status = status$percentile, incl.legend = FALSE)
plot.currentS(year = years, spawners = dat.p$Spawners, currentS = currentS, BMs = annualBMs$SR, status = status$SR, incl.legend = FALSE, labRight = c(expression(italic(S)[GEN1]), expression(italic(S)[MSY])))
mtext(side = 3, adj = 0, line = 0.5, "A")

plot.prod(year = years, a = a, a.year = a.year, status = status$resilience)
mtext(side = 3, adj = 0, line = 0.5, "B")


#------------------------------------------------------------------------------
# Fig. 8: Kingcome River Chinook
# Fig. 9. Klinaklini chinook
# Fig. 10. Nimpkish River Sockeye
#------------------------------------------------------------------------------

# For Chinook and Sockeye, there is no model output, so we just plot the raw
# nuSEDS MAX_ESTIMATE

nuseds <- read.csv("Output/updated_nuseds_2017.csv")

for(p in 1:3){ # For the three populations plotted in the main report
	
	# Select population
	p.name <- c("KINGCOME RIVER", "KLINAKLINI RIVER", "NIMPKISH RIVER")[p]
	sp <- c("Chinook", "Chinook", "Sockeye")[p]
	dat <- subset(nuseds, nuseds$SPECIES == sp & nuseds$WATERBODY == p.name & is.na(nuseds$MAX_ESTIMATE) == FALSE)

	years <- 1950:2017 #Use the same year range for all plots
	n.y <- length(years)
	gen <- 5 # 5-year generation for sockeye and chinook
	spawners <- dat$MAX_ESTIMATE[match(years, dat$ANALYSIS_YR)]
	cbind(years, spawners)	
	
	# Current spawner abundance metric (geometric mean over generation length)
	currentS <- rep(NA, n.y)
	for(i in gen:n.y){
		currentS[i] <- prod(spawners[(i-gen+1):i])^(1/gen)
	}
	
	# Calculate benchmarks each year
	annualBMs <- matrix(NA, nrow = n.y, ncol = 2)
	for(i in which(cumsum(is.na(spawners)==FALSE) == 15):n.y){ # require at least 15 years before calculafting BMs
		annualBMs[i, ] <- quantile(spawners[1:i], c(0.25, 0.75), na.rm=TRUE)
	}

	# Determine status
	status <- rep("white", n.y)
	for(i in 1:n.y){
		if(is.na(currentS[i]) == FALSE){
			if(sum(is.na(annualBMs[i, ])) == 0){
				# Green
				if(currentS[i] > annualBMs[i, 2]){
					status[i] <- statusCols['g']
				} else if(currentS[i] < annualBMs[i, 1]){
					status[i] <- statusCols['r']
				} else {
					status[i] <- statusCols['a']
				}
			} # end benchmarks if
		} # end currentS
	}
	
	
	# Plot
	# quartz(width = 4, height = 2.6, pointsize = 10)
	pdf(file = paste("Results/Fig", p+10, "_", p.name, sp, ".pdf", sep=""), width = 4, height = 2.6, pointsize = 10)
	par(mfrow = c(1,1), mar = c(3,5,2,1), family = font2use)
	plot.currentS(year = years, spawners = spawners, currentS = currentS, BMs = annualBMs, status = status, incl.legend = FALSE)
	dev.off()
} # end p populations
	
	
	