
#
# Example: 
#	Chooser Options
# 
# Descriptions:
#	This script gives examples on some binary options:
#   Simple Chooser Option
#   Complex Chooser Option
#
# Reference:
#	E.G. Haug [1997], Chapter 2
#
# Author:
#	(C) 2002, Diethelm Wuertz, GPL
#


# ------------------------------------------------------------------------------


# Simple Chooser Option:

	SimpleChooserOption(S = 50, X = c(45, 50, 55), Time = 0.5, r = 0.08, 
	  b = 0.08, sigma = 0.25, time = 0.25)

# Complex Chooser Option:

	ComplexChooserOption(S = 50, Xc = 55, Xp = 48, Time = 0.25, 
	  Timec = 0.5, Timep = 0.5833, r = 0.10, b = 0.10-0.05, 
	  sigma = 0.35, doprint = TRUE)
	