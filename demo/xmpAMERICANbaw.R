
#
# Example: 
#	Price American Options Using the BAW Approximation
#	
# Description:
#	The quadratic approximation method by Barone-Adesi and
#	Whaley can be used to price American call and put options 
#	on an underlying asset with cost-of-carry rate b.
#
#	Compare option values of the BAW approximation with the 
#   Black-76 formula, European style. 
#
#   Choose r=0.1, b=0, and X=100 with volatilities sigma of 
#   15%, 25%, and 35%, and prices taking values S of 90, 100, 
#   and 110 for two time periods, Time=0.1 and Time=0.5, 
#   respectively.
#
# Reference:
#	Haug [1997], Chapter 1.4.2, page 22
#   Table 1-3, page 24
#
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#

# ------------------------------------------------------------------------------


# American Calls:

	FuturesPrice = rep(c(90,90,90, 100,100,100, 110,110,110), times = 2)
	Time = c(rep(0.1, times = 9), rep(0.5, times = 9))
	Sigma = rep(c(0.15, 0.25, 0.35), times=6)
    CallPrice = PutPrice = rep(0, times=18)
    for (i in 1:18) {
    	CallPrice[i] = BAWAmericanApproxOption("c", S = FuturesPrice[i], 
    		X = 100, Time = Time[i], r = 0.1, b = 0, sigma = Sigma[i]) 
    	PutPrice[i] = BAWAmericanApproxOption("p", S = FuturesPrice[i], 
    		X = 100, Time = Time[i], r = 0.1, b = 0, sigma = Sigma[i]) }		
    cbind(FuturesPrice, Time, Sigma, CallPrice, PutPrice)
   
# Compare with Black76Option:

    CallPrice = PutPrice = rep(0, times = 18)
    for (i in 1:18) {
    	CallPrice[i] = Black76Option("c", FT = FuturesPrice[i], 
    		X = 100, Time = Time[i], r = 0.1, sigma = Sigma[i])$price
    	PutPrice[i] = Black76Option("p", FT=FuturesPrice[i], 
    		X = 100, Time = Time[i], r = 0.1, sigma = Sigma[i])$price }		
    cbind(FuturesPrice, Time, Sigma, CallPrice, PutPrice)   
    
    