
#
# Example: 
#   Write a perspective plot function to plot the GBS Call and 
#   Put Prices versus Asset Price and Time
#   
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Write the perspective plot function:

	GBSOption3DPlot = 
	function (TypeFlag = c("c", "p"), S, X, Time, r, b, sigma, theta = 30, 
	phi = 30, expand = 0.75, col = "lightblue", ltheta = 120, shade = 0.75, 
	ticktype = "detailed", cex = 0.6, main = "GBS Option Price", ...)
	{   # A function written by Diethelm Wuertz
	     
	    # Description:
	    #   Plot the premium values for a Generalized Black-Scholes 
	    #   option either for a call or a put option.
	    
	    # References:
	    #   Haug E.G., The Complete Guide to Option Pricing Formulas
	    
	    # FUNCTION:
	    
	    # Plotting Premium - 3D: 
	    # x=S, y=Time, z=Premium  
	    TypeFlag = TypeFlag[1] 
	    premium3D = function(S, Time, TypeFlag, X, r, b, sigma) {
	        GBSOption(TypeFlag, S, X, Time, r, b, sigma)$price }
	    
	    # Prices:
	    Price = outer(S, Time, FUN=premium3D, TypeFlag, X, r, b, sigma)
	    
	    # Perspective Plot:
	    persp(x=S, y=Time, z=Price, xlab="S", ylab="Time", zlab=TypeFlag, 
	        theta=theta, phi=phi, expand=expand, col=col, ltheta=ltheta,
	        shade=shade, ticktype=ticktype, cex=cex, main=main, ...) 
	    
	    # Return Value:
	    invisible(list(S = S, Time = Time, Price = Price))  
	}


# BS Call - Plot3D Premium: 
    
    par(mfrow = c(2, 1), cex = 1/3)
    GBSOption3DPlot("c", S = seq(from = 75, to = 125, 
        length = 40), X = 100, Time = seq(from = 1/52, to = 1, 
        length = 40), r = 0.1, b = 0.1, sigma = 0.4)    
        
# BS Put - Plot3D Premium:

    GBSOption3DPlot("p", S = seq(from = 75, to = 125, 
        length = 40), X = 100, Time = seq(from = 1/52, to = 1, 
            length = 40), r = 0.1, b = 0.1, sigma = 0.4)

