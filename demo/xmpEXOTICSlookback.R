
#
# Example: 
#	Lookback Options
# 
# Descriptions:
#	This script gives examples on some lookback options:
#   Floating Strike Lookback Option
#	Fixed Strike Lookback Option
#	Partial-Time Floating Strike Lookback Option
#	Partial-Time Fixed Strike Lookback Option
#
# Reference:
#	E.G. Haug [1997], Chapter 2
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Floating Strike Lookback Option:

   FloatingStrikeLookbackOption(TypeFlag = "c", S = 120, 
     SMinOrMax = 100, Time = 0.5, r = 0.10, b = 0.10-0.06, 
     sigma = 0.30)
  
# Fixed Strike Lookback Option:

   FixedStrikeLookbackOption(TypeFlag = "c", S = 100, 
     SMinOrMax = 100, X = 105, Time = 0.5, r = 0.10, b = 0.10, 
     sigma = 0.30)
   
# Partial Time Floating Strike Lookback Option:

   PTFloatingStrikeLookbackOption(TypeFlag = "p", S = 90, 
     SMinOrMax = 90, time1 = 0.5, Time2 = 1, r = 0.06, b = 0.06, 
     sigma = 0.20, lambda  = 1)
   
# Partial Time Fixed Strike Lookback Option:

   PTFixedStrikeLookbackOption(TypeFlag = "c", S = 100, X = 90, 
	 time1 = 0.5, Time2 = 1, r = 0.06, b = 0.06, sigma = 0.20)

