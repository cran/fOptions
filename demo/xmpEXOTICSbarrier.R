
#
# Example: 
#	Barrier Options
# 
# Descriptions:
#	This script gives examples on some barrier options:
#   Standard Barrier Options
#	Double-Barrier Options
#	Partial-Time Single-Asset Barrier Options
#	Two-Asset Barrier Options
#	Partial-Time Two-Asset Barrier Options
#	Look-Barrier Options
#	Soft-Barrier Options
#
# Reference:
#	E.G. Haug [1997], Chapter 2
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Standard Barrier Option:

   # down-and-out Barrier Call
   StandardBarrierOption(TypeFlag = "cdo", S = 100, X = 90, 
     H = 95, K = 3, Time = 0.5, r = 0.08, b = 0.04, sigma = 0.25)
   
# Double-Barrier Option:

   DoubleBarrierOption(TypeFlag = "co", S = 100, X = 100, L = 50, 
     U = 150, Time = 0.25, r = 0.10, b = 0.10, sigma = 0.15, 
     delta1 = -0.1, delta2 = 0.1)
   
# Partial-Time Single-Asset Barrier Option:

   PTSingleAssetBarrierOption(TypeFlag = "coB1", S = 95, X = 110, 
     H = 100, time1 = 0.5, Time2 = 1, r = 0.20, b = 0.20, 
     sigma = 0.25)
   
# Two-Asset Barrier Option:

   TwoAssetBarrierOption(TypeFlag = "puo", S1 = 100, S2 = 100, 
     X = 110, H = 105, Time = 0.5, r = 0.08, b1 = 0.08, b2 = 0.08, 
     sigma1 = 0.2, sigma2 = 0.2, rho = -0.5)
   
# Partial-Time Two-Asset Barrier Option:

   PTTwoAssetBarrierOption(TypeFlag = "pdo", S1 = 100, S2 = 100, 
     X = 100, H = 85, time1 = 0.5, Time2 = 1, r = 0.1, b1 = 0.1, 
     b2 = 0.1, sigma1 = 0.25, sigma2 = 0.30, rho = -0.5)
   
# Look-Barrier Option:

   LookBarrierOption(TypeFlag = "cuo", S = 100, X = 100, H = 130, 
     time1 = 0.25, Time2 = 1, r = 0.1, b = 0.1, sigma = 0.15)
   LookBarrierOption(TypeFlag = "cuo", S = 100, X = 100, H = 110, 
     time1 = 1, Time2 = 1, r = 0.1, b = 0.1, sigma = 0.30)
   
   
# Soft-Barrier Option:

   SoftBarrierOption(TypeFlag = "cdo", S = 100, X = 100, L = 70, 
     U = 95, Time = 0.5, r = 0.1, b = 0.05, sigma = 0.20)
  
