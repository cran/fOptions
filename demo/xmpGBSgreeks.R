
#
# Example: 
#   Calculate the sensitivities for some selected options
#     
# Reference:
#   Haug [1997], Chapter 1.3, page 11-16
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Delta - Haug [1997], Chapter 1.3.1:
    GBSGreeks("Delta", "c", S = 105, X = 100, Time = 0.5, r = 0.1, 
        b = 0, sigma = 0.36)
    GBSGreeks("Delta", "p", S = 105, X = 100, Time = 0.5, r = 0.1, 
        b = 0, sigma = 0.36)  
    
# Theta - Haug [1997], Chapter 1.3.5:
    GBSGreeks("Theta", "p", S = 430, X = 405, Time = 1/12, r = 0.07, 
        b = 0.07-0.05, sigma = 0.20)
    GBSGreeks("Theta", "p", S = 430, X = 405, Time = 1/365, r = 0.07, 
        b = 0.07-0.05, sigma = 0.20)

# Vega - Haug [1997], Chapter 1.3.4:
    GBSGreeks("Vega", "c", S = 55, X = 60, Time = 0.75, r = 0.10, 
        b = 0.10, sigma = 0.30)

# Rho - Haug [1997], Chapter 1.3.6:
    GBSGreeks("Rho", "c", S = 72, X = 75, Time = 1, r = 0.09, 
        b = 0.09, sigma = 0.19)

# Lambda - Haug [1997], Chapter 1.3.2:
    GBSGreeks("Lambda", "p", S = 105, X = 100, Time = 0.5, r = 0.1, 
        b = 0, sigma = 0.36) 

# Gamma - Haug [1997], Chapter 1.3.3:
    GBSGreeks("Gamma", "c", S = 55, X = 60, Time = 0.75, r = 0.10, 
        b = 0.10, sigma = 0.30)

