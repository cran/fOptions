
#
# Example: 
#	Computing the price of Heston-Nandi Options
#
# Description:
#	Calculate the price of Heston-Nandi Garch(1,1)
#	call and put options. 	
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Set the Model Parameters for the HN Garch(1,1) Option:

	model = list(lambda = -0.5, omega = 5e-6, alpha = 1e-8, 
		beta = 0.7, gamma = 10)
	S = X = 100
   	Time.inDays = 250
   	r.daily = 0.05/250
   	
# List the R Function:

	HNGOption
	
# Compute the Option Price:

	# Call:
	HNGOption(TypeFlag = "c", model, S, X, Time.inDays, r.daily)
	
	# Put:
	HNGOption(TypeFlag = "p", model, S, X, Time.inDays, r.daily)
	
# Compare with the Black-Scholes Formula:

	# Unconditional Variance:
	sigma2.daily = (model$alpha + model$omega) /
		(1-model$beta-model$alpha*model$gamma^2)
	
	# Call:
	GBSOption("c", S = 100, X = 100, Time = 1, r = 0.05,
		b = 0.05, sigma = sqrt(sigma2.daily*250))
	
	# Put:
	GBSOption("p", S = 100, X = 100, Time = 1, r = 0.05,
		b = 0.05, sigma = sqrt(sigma2.daily*250))
	