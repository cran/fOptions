
#
# Example: 
#	Valuate Option by Monte Carlo Simulation
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# How to perform a Monte Carlo Simulation?
   
# First Step:
	   
	# Write a function to generate the option's innovations. 
	# Use scrambled normal Sobol numbers:
	sobolInnovations = function(mcSteps, pathLength, init, ...) {
		# Create Normal Sobol Innovations:
	   	innovations = rnorm.sobol(mcSteps, pathLength, init, ...)
	   	# Return Value:
	   	innovations }
   	
# Second Step: 
   
	# Write a function to generate the option's price paths.  
   	# Use a Wiener path:
   	wienerPath = function(eps) {	
		# Note, the option parameters must be globally defined!
		# Generate the Paths:
		path = (b-sigma*sigma/2)*delta.t + sigma*sqrt(delta.t)*eps
		# Return Value:
     	path }
      
## Third Step: 
   
	# Write a function for the option's payoff
   
   	# Example 1: use the payoff for a plain Vanilla Call or Put:
   	plainVanillaPayoff = function(path) { 
	 	# Note, the option parameters must be globally defined!
	 	# Compute the Call/Put Payoff Value:
	 	ST = S*exp(sum(path))
	 	if (TypeFlag == "c") payoff = exp(-r*Time)*max(ST-X, 0)
		if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-ST)
	 	# Return Value:
	 	payoff }
   
   	# Example 2: use the payoff for an arithmetic Asian Call or Put:
   	arithmeticAsianPayoff = function(path) { 
	 	# Note, the option parameters must be globally defined!
	 	# Compute the Call/Put Payoff Value:
	 	SM = mean(S*exp(cumsum(path)))
	 	if (TypeFlag == "c") payoff = exp(-r*Time)*max(SM-X, 0)
	 	if (TypeFlag == "p") payoff = exp(-r*Time)*max(0, X-SM)
	 	# Return Value:
	 	payoff }

# Final Step: 
   
	# Set Global Parameters for the plain Vanilla / arithmetic Asian Options:
   	TypeFlag <<- "c"; S <<- 2; X <<- 2
   	Time <<- 1/12; sigma <<- 0.5; r <<- 0.05; b <<- 0.05
   
   	# Do the Asian Simulation with scrambled random numbers:
   	# Short run:
   	mc = MonteCarloOption(delta.t = 1/360, pathLength = 30, mcSteps = 1000, 
     	mcLoops = 50, init = TRUE, innovations.gen = sobolInnovations, 
     	path.gen = wienerPath, payoff.calc = arithmeticAsianPayoff, 
     	antithetic = TRUE, standardization = FALSE, trace = TRUE, 
     	scrambling = 2, seed = 4711)
   
	# Plot the MC Iteration Path:
	par(mfrow = c(1, 1), cex = 0.7)
	mcPrice = cumsum(mc)/(1:length(mc))
	plot(mcPrice, type = "l", main = "Arithmetic Asian Option", 
		xlab = "Monte Carlo Loops", ylab = "Option Price")

	# Turnbull Wakeman Asian Approximation:
	TurnWakeAsianApproxOption(TypeFlag = "c", S = 2, SA = 2, 
   	  X = 2, Time = 1/12, time = 1/12, tau = 0 , r = 0.05, b = 0.05, 
   	  sigma = 0.5)$price
    
# A Long Run:

	# mc = MonteCarloOption(delta.t = 1/360, pathLength = 360, mcSteps = 1000, 
    #  	mcLoops = 1000, init = TRUE, innovations.gen = sobolInnovations, 
    # 	path.gen = wienerPath, payoff.calc = arithmeticAsianPayoff, 
    # 	antithetic = TRUE, standardization = FALSE, trace = TRUE, 
    # 	scrambling = 2, seed = 4711)
    
    # 0.24668  this Monte Carlo Run
   	# 0.24979  Turnbull Wakeman Approximation
   	# 0.248    Geman Eydeland
   	# 0.247    Euler PDE
   	# 0.247    Post-Widder PDE
   	# 0.24642  Fourier Transformation
   	# 0.24640  Linetzky's Approach
   	
	
 
 
