
#
# Example: 
#   Calculate the option prices for
#   1) an European Options on a Stock with Cash Dividends,
#   2) an Option on Stock Indexes,
#   3) an Option on Futures, and
#   4) a Currency Option.
#
# Reference:
#   Haug [1997], Chapter 1.1, page 2-6
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# European Option on a Stock with Cash Dividends:

    S = 100 - 2*exp(-0.10*0.25) - 2*exp(-0.10*0.50)
    r = 0.10
    GBSOption("c", S = S, X = 90, Time = 0.75, r = r, b = r, 
        sigma = 0.25) 
    
# Option on Stock Indexes:

    r = 0.10
    q = 0.05
    GBSOption("p", S = 100, X = 95, Time = 0.5, r = r, b = r-q, 
        sigma = 0.20) 

# Option on Futures:

    FuturesPrice = 19
    b = 0
    GBSOption("c", S = FuturesPrice, X = 19, Time = 0.75, r = 0.1, 
        b = b, sigma = 0.28) 
    GBSOption("p", S = FuturesPrice, X = 19, Time = 0.75, r = 0.1, 
        b = b, sigma = 0.28) 
    
# Currency Option:

    r = 0.06
    rf = 0.08
    GBSOption("c", S = 1.56, X = 1.60, Time = 0.5, r = r, b = r-rf, 
        sigma = 0.12) 

