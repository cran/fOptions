
#
# Example: 
#	Asian Options
# 
# Descriptions:
#	This script gives examples on some Asian options:
#   Geometric Average Rate Option
#   Approximations for the Arithmetic Average Rate Option
#	  Turnbull's and Wakeman's Approximation
#	  Levy's Approximation
#
# Reference:
#	E.G. Haug [1997], Chapter 2
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Geometric Average Rate Option:

   GeometricAverageRateOption("p", S = 80, X = 85, 
     Time = 0.25, r = 0.05, b = 0.08, sigma = 0.20)

# Turnbull Wakeman Approximation:

   TurnWakeAsianApproxOption("p", S = 90, SA = 88, 
     X = 95, Time = 0.50, time = 0.25, tau = 0.0, r = 0.07, 
     b = 0.02, sigma = 0.25)

# Levy Asian Approximation:   

   LevyAsianApproxOption("c", S = 100, SA = 100, X = 105, 
     Time = 0.75, time = 0.50, r = 0.10, b = 0.05, sigma = 0.15)
     	
     	