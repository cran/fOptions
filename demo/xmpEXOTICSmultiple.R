
#
# Example: 
#	Multiple Assets Options
# 
# Descriptions:
#	This script gives examples on some multiple assets options:
# 	Two Asset Correlation Options
# 	European Exchange Options
# 	American Exchange Options
# 	Exchange Options On Exchange Options
# 	Two Risky Assets Options
# 	Spread-Option Approximation
#
# Reference:
#	E.G. Haug [1997], Chapter 2
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Two Asset Correlation Options:
   	
   	TwoAssetCorrelationOption(TypeFlag = "c", S1 = 52, S2 = 65, 
     	X1 = 50, X2 = 70, Time = 0.5, r = 0.10, b1 = 0.10, b2 = 0.10, 
     	sigma1 = 0.2, sigma2 = 0.3, rho = 0.75) 

# European Exchange Options: 

   	EuropeanExchangeOption(S1 = 22, S2 = 0.20, Q1 = 1, Q2 = 1, 
     	Time = 0.1, r = 0.1, b1 = 0.04, b2 = 0.06, sigma1 = 0.2, 
     	sigma2 = 0.25, rho = -0.5)
	 
# American Exchange Options:
 
   	AmericanExchangeOption(S1 = 22, S2 = 0.20, Q1 = 1, Q2 = 1, 
    	Time = 0.1, r = 0.1, b1 = 0.04, b2 = 0.06, sigma1 = 0.2, 
     	sigma2 = 0.25, rho = -0.5)

# Exchange Options On Exchange Options:

   	for (flag in 1:4) print(
   		ExchangeOnExchangeOption(TypeFlag = as.character(flag), 
     		S1 = 105, S2 = 100, Q = 0.1, time1 = 0.75, Time2 = 1.0, 
     		r = 0.1, b1 = 0.10, b2 = 0.10, sigma1 = 0.20, 
     		sigma2 = 0.25, rho = -0.5))

# Two Risky Assets Options:

   	TwoRiskyAssetsOption(TypeFlag = "cmax", S1 = 100, S2 = 105, 
     	X = 98, Time = 0.5, r = 0.05, b1 = -0.01, b2 = -0.04, 
     	sigma1 = 0.11, sigma2 = 0.16, rho = 0.63)
   	TwoRiskyAssetsOption(TypeFlag = "pmax", S1 = 100, S2 = 105, 
     	X = 98, Time = 0.5, r = 0.05, b1 = -0.01, b2 = -0.04, 
     	sigma1 = 0.11, sigma2 = 0.16, rho = 0.63)

# Spread-Option Approximation:

   	SpreadApproxOption(TypeFlag = "c", S1 = 28, S2 = 20, X = 7, 
     	Time = 0.25, r = 0.05, sigma1 = 0.29, sigma2 = 0.36, 
     	rho = 0.42)
	  	