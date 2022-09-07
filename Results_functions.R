###############################################################################
# Functions for benchmark calculation
###############################################################################
# Function to calculate Smsy
calcSmsy <- function(a, b) {
	Smsy <- (1 - lambert_W0(exp(1 - a))) / b
	return(as.numeric(Smsy))
}

# Function to optimize Sgen
Sgen.optim <- function (Sgen.hat, theta, Smsy) {
	a <- theta[1]
	b <- theta[2]
	sig <- exp(theta[3])
	Smsy.hat <- Sgen.hat * exp(a - b * Sgen.hat) 
	epsilon <- log(Smsy) - log(Smsy.hat)
	nloglike <- - sum(dnorm(x = epsilon, mean = 0, sd = sig, log = TRUE))	
	return(nloglike)
}

# Function to calculate Sgen
calcSgen <- function(Sgen.hat, theta, Smsy) {
	fit <- optimize(f = Sgen.optim, interval = c(0, Smsy), theta = theta, Smsy = Smsy)
	if(round(fit$minimum) == 0) warning("Sgen1 at lower bound of zero")
	if(round(fit$minimum) == round(Smsy)){
		warning("Lower benchmark greater than upper benchmark (Sgen1 > Smsy). Set to NA.")
		return(Sgen1 = NA)
	} else {
		return(Sgen1 = as.numeric(fit$minimum))
	}
}

#------------------------------------------------------------------------------
# Plot current abundance metric and BMs
#------------------------------------------------------------------------------
plot.currentS <- function(year, spawners, currentS, BMs, status, incl.legend = "topright", labRight = c(expression(italic(S)[25]), expression(italic(S)[75]))){
	
	plot(year, spawners * 10^-3, "n", bty = "l", las = 1, xlab = "", ylab = "Spawners (thousands)")
	u <- par('usr')
	lines(year, spawners* 10^-3, "o", col = grey(0.7), bg = "white")
	
	lines(year, BMs[, 1]* 10^-3, lty = 2)
	lines(year, BMs[, 2]* 10^-3, lty = 2)
	
	lines(year, currentS* 10^-3, lwd = 1.5)
	points(year, currentS* 10^-3, pch = 21, bg = status)
	
	text(rep(max(year), 2), BMs[which(year == max(year)), ]*10^-3, labRight, xpd = NA, pos = 4, cex = 0.8)
	
	if(incl.legend != FALSE){
		legend(incl.legend, lty = c(1, 1, 2), col = c(grey(0.8), 1, 1), pch = c(NA, 21, NA), pt.bg=c(NA, "white", NA), lwd = c(1.2, 1, 1),  c("Spawners", "Metric: generational average", "Benchmarks"), cex = 0.8, bg = "white")
	}
	
}


#------------------------------------------------------------------------------
# Plot resilience metric and BMs
#------------------------------------------------------------------------------
plot.prod <- function(year, a, a.year, status){
	# year : all years to include on plot (matches spawners)
	# a : model output for productivity; 3 columns mean, lci, uci
	# a.year : which index of years does a correspond to?
	# status : what is the status col for each year?
	plot(year[a.year], exp(a[, 1]), "l", log = 'y', ylim = exp(c(min(a[, 2], na.rm = TRUE), max(a[, 3], na.rm = TRUE))), xlim = range(year), las = 1, bty = "l", xlab = "", ylab = "Recruits per spawner")
	
	for(i in 1:length(a.year)){
		if(i == 1){
			polygon(x = rep(c(year[a.year[i]] - 0.5, year[a.year[i]] + 0.5), each = 2), y = exp(c(a[i, 2], a[i, 3], mean(a[c(i, i+1), 3]), mean(a[c(i, i+1), 2]))), col = status[a.year[i]], border = NA)
		} else if (i == length(a.year)) {
			polygon(x = rep(c(year[a.year[i]] - 0.5, year[a.year[i]] + 0.5), each = 2), y = exp(c(mean(a[c(i-1, i), 2]), mean(a[c(i-1, i), 3]), a[i, 3], a[i, 2])), col = status[a.year[i]], border = NA)
		} else if (year[a.year[i-1]] != (year[a.year[i]] - 1)){
			polygon(x = rep(c(year[a.year[i]] - 0.5, year[a.year[i]] + 0.5), each = 2), y = exp(c(a[i, 2], a[i, 3], mean(a[c(i, i+1), 3]), mean(a[c(i, i+1), 2]))), col = status[a.year[i]], border = NA)
		} else if ((year[a.year[i+1]] != (year[a.year[i]] + 1))) {
			polygon(x = rep(c(year[a.year[i]] - 0.5, year[a.year[i]] + 0.5), each = 2), y = exp(c(mean(a[c(i-1, i), 2]), mean(a[c(i-1, i), 3]), a[i, 3], a[i, 2])), col = status[a.year[i]], border = NA)
		} else {
			polygon(x = rep(c(year[a.year[i]] - 0.5, year[a.year[i]] + 0.5), each = 2), y = exp(c(mean(a[c(i-1, i), 2]), mean(a[c(i-1, i), 3]), mean(a[c(i, i+1), 3]), mean(a[c(i, i+1), 2]))), col = status[a.year[i]], border = NA)
		}
	} # end i years
	
	lines(year[a.year], exp(a[, 1]), lwd = 1.2)
	abline(h = 1, lty = 2)
} # end plot.prod function
