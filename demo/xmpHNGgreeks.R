
#
# Example: 
#	Computing the Greeks of Heston-Nandi Options
#
# Description:
#	Calculate the sensitiviteis delta and gamma of 
#	Heston-Nandi Garch(1,1) call and put options. 	
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Option Parameters:

	S = X = 100
	Time.inDays = 125
	r.daily = 0.05/250
	model = list(lambda = -0.5, omega = 2.3e-6, alpha = 2.9e-6, 
		beta = 0.85, gamma = 184.25)
	sigma.daily = sqrt((model$alpha + model$omega) /
		(1 - model$beta - model$alpha*model$gamma^2))
		
	# Print:
	S; X; Time.inDays; r.daily; sigma.daily
	model

# Compute the Delta 
# and compare the result with the finite dfference approaximation:

	Summary = NULL
	# Call:
	Greek = HNGGreeks("Delta", "c", model, S, X, Time.inDays, r.daily)
	C1 = HNGOption("c", model, S*0.9999, X, Time.inDays, r.daily)$price
	C2 = HNGOption("c", model, S*1.0001, X, Time.inDays, r.daily)$price
	numGreek = (C2-C1)/(0.0002*S)
	Diff = 100*(Greek-numGreek)/Greek
	Summary = rbind(Summary, c(Greek, numGreek, Diff))
	# Put:
	Greek = HNGGreeks("Delta", "p", model, S, X, Time.inDays, r.daily)
	C1 = HNGOption("p", model, S*0.9999, X, Time.inDays, r.daily)$price
	C2 = HNGOption("p", model, S*1.0001, X, Time.inDays, r.daily)$price
	numGreek = (C2-C1)/(0.0002*S)
	Diff = 100*(Greek-numGreek)/Greek
	Summary = rbind(Summary, c(Greek, numGreek, Diff))
	# Print:
	dimnames(Summary) <- 
		list(c("Call", "Put"), c("Greek", "numGreek", "Diff"))
	Summary
	
# Do the same for the Gamma Sensitivity ...

	Summary = NULL
	# Call:
	Greek = HNGGreeks("Gamma", "c", model, S, X, Time.inDays, r.daily)
	C0 = HNGOption("c", model, S, X, Time.inDays, r.daily)$price
	C1 = HNGOption("c", model, S*0.9999, X, Time.inDays, r.daily)$price 
	C2 = HNGOption("c", model, S*1.0001, X, Time.inDays, r.daily)$price 	
	numGreek = (C2 - 2*C0 + C1)/(0.0002*S/2)^2
	Diff = 100*(Greek-numGreek)/Greek	
	Summary = rbind(Summary, c(Greek, numGreek, Diff))
	# Put:
	Greek = HNGGreeks("Gamma", "p", model, S, X, Time.inDays, r.daily)
	C0 = HNGOption("p", model, S, X, Time.inDays, r.daily)$price
	C1 = HNGOption("p", model, S*0.9999, X, Time.inDays, r.daily)$price 
	C2 = HNGOption("p", model, S*1.0001, X, Time.inDays, r.daily)$price 
	numGreek = (C2 - 2*C0 + C1)/(0.0002*S/2)^2
	Diff = 100*(Greek-numGreek)/Greek	
	Summary = rbind(Summary, c(Greek, numGreek, Diff))
	# Print:
	dimnames(Summary) <- 
		list(c("Call", "Put"), c("Greek", "numGreek", "Diff"))
	Summary

		