
#
# Example: 
#   Write a perspective plot function to plot the GBS Greeks 
#   for Call and Put Prices versus Asset Price and Time
#
# Description:
#   Plot the sensitivities Delta, Theta, Vega, Rho, Lambda and 
#   Gamma as a function of the scaled asset price S/X and the 
#   time-scaled volatility sigma^2 T. 
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Write the perspective plot function:

	GBSGreeks3DPlot = 
	function (Selection = c("Delta", "Theta", "Vega", "Rho", "Lambda", "Gamma",
	"CofC"), TypeFlag = c("c", "p"), S, X, Time, r, b, sigma, theta = 30, 
	phi = 30, expand = 0.75, col = "lightblue", ltheta = 120, shade = 0.75, 
	ticktype = "detailed", cex = 0.6, main = "GBS Option Sensitivity", ...)
	{   # A function written by Diethelm Wuertz
	     
	    # Description:
	    #   Plot the Greeks for a Generalized Black-Scholes 
	    #   option either for a call or a put option.
	    
	    # References:
	    #   Haug E.G., The Complete Guide to Option Pricing Formulas
	    
	    # FUNCTION:
	    
	    # Plotting Greeks - 3D: 
	    # x = S, y = Time, z = Greek    
	    
	    # Selection:
	    # "delta", "theta", "vega", "rho", "lambda", "gamma", "cofc"
	    greeks3D = function(S, Time, Selection, TypeFlag, X, r, b, sigma) 
	        { GBSGreeks(Selection, TypeFlag, S, X, Time, r, b, sigma)}
	    
	    # Sensitivities:
	    Greeks = outer(S, Time, FUN=greeks3D, Selection, TypeFlag, X, 
	        r, b, sigma)
	    
	    # Perspective Plot:
	    persp(x=S, y=Time, z=Greeks, xlab="S", ylab="Time", zlab=Selection, 
	        theta=theta, phi=phi, expand=expand, col=col, ltheta=ltheta,
	        shade=shade, ticktype=ticktype, cex=cex, main=main, ...) 
	    
	    # Return Value:
	    invisible(list(S = S, Time = Time, Sensitivity = Greeks))
	}


# BS - Plot3D Greeks:
    
    # pdf(file = "xmpGBSgreeks3D.pdf", width = 7, height = 9)
    par(mfrow = c(3, 2), cex = 1/3)
    for (Selection in c("delta", "theta", "vega", "rho", "lambda", "gamma"))
        GBSGreeks3DPlot(Selection, "c", 
            S = seq(from = 75, to = 125, length = 25), X = 100,
            Time = seq(from = 1/52, to = 1, length = 25), r = 0.1, 
            b = 0.1, sigma = 0.40)
    # dev.off()

