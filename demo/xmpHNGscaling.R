
#
# Example: 
#   HN Options Pricing - Scaling of Prices
#
# Description:
#	The example shows for the HN Garch(1,1) model 
#	the scaling in price/strike and volatility*time.
#
# Function Call: 
# 	HNGOption(), GBSOption
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Initial Parameter Setting:

   	scales = c(1/2, 1, 2)
   	Model = list(lambda=-0.5, omega=1e-6, alpha=1e-6, beta=0.5, gamma=0)
 	par (mfrow = c(2, 2), cex = 0.7)

# Price/Strike Variation:

	S  = seq(75, 125, by = 5); X = 100
	Time = 126 
	
 	# HNG:	
	plot(x = c(0.75, 1.25), y = c(0, 28), type = "n", 
		xlab = "S/X", ylab = "Call Price", 
		main = "HN: Volatility*Time fixed")
 	for (j in 1:length(scales)) {
	 	model = Model
		model$omega = Model$omega*scales[j]
		model$alpha = Model$alpha*scales[j]
      	for (i in 1:length(S)) {
	      	x = S[i]/X
   			y = HNGOption("c", model, S[i], X, Time/scales[j], 0)$price
        	points(x, y, col = j, pch = (j+2)) } }
    legend(0.8, 26, legend = c("scale = 1/2", "scale = 1", "scale = 2"), 
		pch = "\3\4", bty = "n", col = 1:3)
        	
	# BS:
	plot(x = c(0.75, 1.25), y = c(0, 28), type = "n", 
		xlab = "S/X", ylab = "Call Price", 
		main = "BS: Volatility*Time fixed")
 	for (j in 1:length(scales)) {
	 	model = Model
		model$omega = Model$omega/scales[j]
		model$alpha = Model$alpha/scales[j]
		sigma = sqrt((model$alpha+model$omega)/(1-model$beta))
		print(sigma^2 * Time*scales[j])
      	for (i in 1:length(S)) {
	      	x = S[i]/X
   			y = GBSOption("c", S[i], X, Time*scales[j], r=0, b=0, 
   				sigma=sigma)$price
        	points(x, y, col = j, pch = (j+2)) } }	
	legend(0.8, 26, legend = c("scale = 1/2", "scale = 1", "scale = 2"), 
		pch = "\3\4", bty = "n", col = 1:3)
		

# Volatility*Time Variation:

	S = 100; X = 100
	Time = c((12:1)*21, 5, 1) 
	sigma = sqrt((Model$alpha+Model$omega)/(1-Model$beta)); 252*sigma

 	# HNG:		
	plot(x = c(-252, 0) , y = c(1.5, 0), type = "n", xlab = "Days", 
		ylab = "Put Price / scale", main = "HN: S/X fixed")
 	for ( j in 1:length(scales) ) {
	 	S.scaled = S*scales[j]; X.scaled = X*scales[j]	
   		for (i in 1:length(Time)) {
   			x = -Time[i]
   			y = HNGOption("c", Model, S.scaled, X.scaled, Time[i], 0)$price /
   				scales[j]
   			points(x, y, col = j, pch = (j+2)) } }
	legend(-240, 0.6, legend = c("scale = 0.5", "scale = 1", "scale = 2"), 
		pch = "\3\4", bty = "n", col = 1:3)
		
			
	# BS:
	plot(x = c(-252, 0) , y = c(1.5, 0), type = "n", xlab = "Days", 
		ylab = "Put Price / scale ", main = "BS: S/X fixed")
	for (j in 1:length(scales) ) {
	 	S.scaled = S*scales[j]; X.scaled = X*scales[j]	
   		for (i in 1:length(Time)) {
   			x = -Time[i]
   			y = GBSOption("c", S.scaled, X.scaled, Time[i], 0, 0, sigma)$price /
   				scales[j]
   			points(x, y, col = j, pch = (j+2)) } }
	legend(-240, 0.6, legend = c("scale = 0.5", "scale = 1", "scale = 2"), 
		pch = "\3\4", bty = "n", col = 1:3)

		
	