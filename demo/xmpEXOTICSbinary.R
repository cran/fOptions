
#
# Example: 
#	Exotics - Binary Options
#  
# Descriptions:
#	This script gives examples on some binary options:
#   Gap Option
#   Cash-Or-Nothing Option
#   Two-Asset Cash-Or-Nothing Option
#   Asset-Or-Nothing Option
#	Super-Share Option
#
# Reference:
#	E.G. Haug [1997], Chapter 2
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Gap Option:

	GapOption("c", S = 50, X1 = 50, X2 = 57, Time = c(0.25, 0.50, 1), 
		r = 0.09, b = 0.09, sigma = 0.20)

# Cash-Or-Nothing Option:

	CashOrNothingOption("p", S = 100, X = 80, K = 10, Time = 0.75, 
		r = 0.06, b = 0, sigma = c(0.15, 0.25, 0.35))
		
# Two-Asset Cash-Or-Nothing Option:

    for (TypeFlag in c("c", "p", "ud", "du"))
	print(TwoAssetCashOrNothingOption(TypeFlag, S1 = 100, S2 = 100, 
		X1 = 110, X2 = 90, K = 10, Time = 0.5, r = 0.10, b1 = 0.05, 
		b2 = 0.06, sigma1 = 0.20, sigma2 = 0.25, rho = 0))
		
# Asset-Or-Nothing Option:

	AssetOrNothingOption("p", S = 70, X = 65, Time = 0.5, r = 0.07, 
		b = 0.07-0.05, sigma = 0.27)		
	
# Super-Share Option:

	SuperShareOption(S = 100, XL = 90, XH = 110, Time = 0.25, 
		r = 0.10, b = 0, sigma = 0.20)				
		
		